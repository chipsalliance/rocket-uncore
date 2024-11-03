package org.chipsalliance.uncore.dm

import chisel3._
import chisel3.util.{log2Up, BitPat, Cat, DecoupledIO, UIntToOH}
import chisel3.experimental.dataview.DataViewable
import chisel3.experimental.hierarchy.{instantiable, Instance, Instantiate}
import chisel3.experimental.{SerializableModule, SerializableModuleParameter}
import chisel3.probe.{define, Probe, ProbeValue}
import chisel3.properties.{AnyClassType, Class, Property}

import org.chipsalliance.amba.RegMapper.regmap
import org.chipsalliance.amba._
import org.chipsalliance.amba.axi4.bundle._

import org.chipsalliance.uncore.dm.DMI_RegAddrs._
import org.chipsalliance.uncore.dm.DsbRegAddrs._
import org.chipsalliance.uncore.dm.DsbBusConsts._

class TLDebugModuleInnerAsyncInterface(parameter: DMParameter) extends Bundle {
  val i_clock = Input(Clock())
  val i_reset = Input(if (parameter.useAsyncReset) AsyncReset() else Bool())
  // val dmiNode = Input(Flipped(new DMIIO(parameter)))
  val i_dmiNode = Input(
    Flipped(axi4.bundle.verilog.irrevocable(parameter.axi4parameter)).asInstanceOf[AXI4RWIrrevocableVerilog]
  )
  val i_tlNode = Input(
    Flipped(axi4.bundle.verilog.irrevocable(parameter.axi4parameter)).asInstanceOf[AXI4RWIrrevocableVerilog]
  )

  /** dm reset signal passed in from Outer */
  val i_dmactive = Input(Bool())

  /** conrol signals for Inner
    *
    * generated in Outer
    */
  val i_innerCtrl = Flipped(new DecoupledIO(new DebugInternalBundle(parameter.nComponents)))

  /** debug unavail signal passed in from Outer */
  val i_debugUnavail = Input(Vec(parameter.nComponents, Bool()))

  /** debug interruption from Inner to Outer
    *
    * contain 2 type of debug interruption causes:
    *   - halt group
    *   - halt-on-reset
    */
  val i_hgDebugInt = Output(Vec(parameter.nComponents, Bool()))

  /** interface for trigger */
  val extTrigger = Option.when(parameter.nExtTriggers > 0)(new DebugExtTriggerIO(parameter.nExtTriggers))

  /** vector to indicate which hart is in reset
    *
    * dm receives it from core and sends it to Inner
    */
  val hartIsInReset = Input(Vec(parameter.nComponents, Bool()))

  /** Debug Authentication signals from core */
  val i_auth = Option.when(parameter.hasAuthentication)(new DebugAuthenticationIO())
  val i_rf_reset = Input(Reset()) // RF transform
}

// Wrapper around TL Debug Module Inner and an Async DMI Sink interface.
// Handles the synchronization of dmactive, which is used as a synchronous reset
// inside the Inner block.
// Also is the Sink side of hartsel & resumereq fields of DMCONTROL.
class TLDebugModuleInnerAsync(val parameter: DMParameter)
    extends FixedIORawModule(new TLDebugModuleInnerAsyncInterface(parameter))
    with SerializableModule[DMParameter]
    with ImplicitClock
    with ImplicitReset {
  override protected def implicitClock: Clock = io.i_clock
  override protected def implicitReset: Reset = io.i_reset
  val cfg = parameter
  val supportHartArray = cfg.supportHartArray & (cfg.nComponents > 1)
  val nExtTriggers = cfg.nExtTriggers
  val nHaltGroups =
    if ((cfg.nComponents > 1) | (cfg.nExtTriggers > 0)) cfg.nHaltGroups
    else 0 // no halt groups possible if single hart with no external triggers

  val hartSelFuncs =
    if (cfg.nComponents > 1)
      DebugModuleHartSelFuncs(
        hartIdToHartSel = (x) => x,
        hartSelToHartId = (x) => x
      )
    else
      DebugModuleHartSelFuncs(
        hartIdToHartSel = (x) => 0.U,
        hartSelToHartId = (x) => x
      )

  // sb2tlOpt.map { sb =>
  //   sb.module.clock := io.tl_clock
  //   sb.module.reset := io.tl_reset
  //   sb.module.rf_reset := io.tl_reset
  // }

  // --------------------------------------------------------------
  // Import constants for shorter variable names
  // --------------------------------------------------------------

  import DMI_RegAddrs._
  import DsbRegAddrs._
  import DsbBusConsts._

  // --------------------------------------------------------------
  // Sanity Check Configuration For this implementation.
  // --------------------------------------------------------------

  require(cfg.supportQuickAccess == false, "No Quick Access support yet")
  require((nHaltGroups > 0) || (nExtTriggers == 0), "External triggers require at least 1 halt group")

  // --------------------------------------------------------------
  // Register & Wire Declarations (which need to be pre-declared)
  // --------------------------------------------------------------

  // run control regs: tracking all the harts
  // implements: see implementation-specific bits part
  /** all harts halted status */
  val haltedBitRegs = Reg(UInt(cfg.nComponents.W))

  /** all harts resume request status */
  val resumeReqRegs = Reg(UInt(cfg.nComponents.W))

  /** all harts have reset status */
  val haveResetBitRegs = Reg(UInt(cfg.nComponents.W))

  // default is 1,after resume, resumeAcks get 0
  /** all harts resume ack status */
  val resumeAcks = Wire(UInt(cfg.nComponents.W))

  // --- regmapper outputs

  // hart state Id and En
  // in Hart Bus Access ROM
  val hartHaltedWrEn = Wire(Bool())
  val hartHaltedId = Wire(UInt(sbIdWidth.W))
  val hartGoingWrEn = Wire(Bool())
  val hartGoingId = Wire(UInt(sbIdWidth.W))
  val hartResumingWrEn = Wire(Bool())
  val hartResumingId = Wire(UInt(sbIdWidth.W))
  val hartExceptionWrEn = Wire(Bool())
  val hartExceptionId = Wire(UInt(sbIdWidth.W))

  // progbuf and abstract data: byte-addressable control logic
  // AccessLegal is set only when state = waiting
  // RdEn and WrEnMaybe : contrl signal drived by DMI bus
  val dmiProgramBufferRdEn = WireInit(VecInit(Seq.fill(cfg.nProgramBufferWords * 4) { false.B }))
  val dmiProgramBufferAccessLegal = WireInit(false.B)
  val dmiProgramBufferWrEnMaybe = WireInit(VecInit(Seq.fill(cfg.nProgramBufferWords * 4) { false.B }))

  val dmiAbstractDataRdEn = WireInit(VecInit(Seq.fill(cfg.nAbstractDataWords * 4) { false.B }))
  val dmiAbstractDataAccessLegal = WireInit(false.B)
  val dmiAbstractDataWrEnMaybe = WireInit(VecInit(Seq.fill(cfg.nAbstractDataWords * 4) { false.B }))

  // --------------------------------------------------------------
  // Registers coming from 'CONTROL' in Outer
  // --------------------------------------------------------------

  val i_dmAuthenticated = io.i_auth.map(a => a.dmAuthenticated).getOrElse(true.B)

  val selectedHartReg = Reg(UInt(cfg.MaxHartIdBits.W))
  // hamaskFull is a vector of all selected harts including hartsel, whether or not supportHartArray is true
  val hamaskFull = WireInit(VecInit(Seq.fill(cfg.nComponents) { false.B }))

  if (cfg.nComponents > 1) {
    when(~io.i_dmactive) {
      selectedHartReg := 0.U
    }.elsewhen(io.i_innerCtrl.fire) {
      selectedHartReg := io.i_innerCtrl.bits.hartsel
    }
  }

  if (supportHartArray) {
    val hamaskZero = WireInit(VecInit(Seq.fill(cfg.nComponents) { false.B }))
    val hamaskReg = Reg(Vec(cfg.nComponents, Bool()))
    when(~io.i_dmactive || ~i_dmAuthenticated) {
      hamaskReg := hamaskZero
    }.elsewhen(io.i_innerCtrl.fire) {
      hamaskReg := Mux(io.i_innerCtrl.bits.hasel, io.i_innerCtrl.bits.hamask, hamaskZero)
    }
    hamaskFull := hamaskReg
  }
  // Outer.hamask doesn't consider the hart selected by dmcontrol.hartsello,
  // so append it here
  when(selectedHartReg < cfg.nComponents.U) {
    hamaskFull(if (cfg.nComponents == 1) 0.U(0.W) else selectedHartReg) := true.B
  }

  io.i_innerCtrl.ready := true.B

  // Construct a Vec from io.i_innerCtrl fields indicating whether each hart is being selected in this write
  // A hart may be selected by hartsel field or by hart array
  val hamaskWrSel = WireInit(VecInit(Seq.fill(cfg.nComponents) { false.B }))
  for (component <- 0 until cfg.nComponents) {
    hamaskWrSel(component) := ((io.i_innerCtrl.bits.hartsel === component.U) ||
      (if (supportHartArray) io.i_innerCtrl.bits.hasel && io.i_innerCtrl.bits.hamask(component) else false.B))
  }

  // -------------------------------------
  // Halt-on-reset logic
  //  hrmask is set in dmOuter and passed in
  //  Debug interrupt is generated when a reset occurs whose corresponding hrmask bit is set
  //  Debug interrupt is maintained until the hart enters halted state
  // -------------------------------------
  val hrReset = WireInit(VecInit(Seq.fill(cfg.nComponents) { false.B }))
  val hrDebugInt = Wire(Vec(cfg.nComponents, Bool()))
  val i_hrmaskReg = RegInit(hrReset)
  val hartIsInResetSync = Wire(Vec(cfg.nComponents, Bool()))

  for (component <- 0 until cfg.nComponents) {
    hartIsInResetSync(component) := io.hartIsInReset(component) // TODO: AsyncResetSynchronizerShiftReg
  }

  when(~io.i_dmactive || ~i_dmAuthenticated) {
    i_hrmaskReg := hrReset
  }.elsewhen(io.i_innerCtrl.fire) {
    i_hrmaskReg := io.i_innerCtrl.bits.hrmask
  }

  // withReset(reset.asAsyncReset) {          // ensure interrupt requests are negated at first clock edge
  val hrDebugIntReg = RegInit(VecInit(Seq.fill(cfg.nComponents) { false.B }))
  when(~io.i_dmactive || ~i_dmAuthenticated) {
    hrDebugIntReg := hrReset
  }.otherwise {
    hrDebugIntReg := i_hrmaskReg.asUInt &
      (hartIsInResetSync.asUInt | // set debugInt during reset
        (hrDebugIntReg.asUInt & ~(haltedBitRegs.asUInt))) // maintain until core halts
  }
  hrDebugInt := hrDebugIntReg
  // }

  // --------------------------------------------------------------
  // DMI Registers
  // --------------------------------------------------------------

  // ----DMSTATUS

  val DMSTATUSRdData = WireInit(0.U.asTypeOf(new DMSTATUSFields()))
  DMSTATUSRdData.authenticated := i_dmAuthenticated
  DMSTATUSRdData.version := 2.U // Version 0.13
  io.i_auth.map(a => DMSTATUSRdData.authbusy := a.dmAuthBusy)

  val resumereq = io.i_innerCtrl.fire && io.i_innerCtrl.bits.resumereq

  when(i_dmAuthenticated) {
    DMSTATUSRdData.hasresethaltreq := true.B

    DMSTATUSRdData.anynonexistent := (selectedHartReg >= cfg.nComponents.U) // only hartsel can be nonexistent

    // all harts nonexistent if hartsel is out of range and there are no harts selected in the hart array
    DMSTATUSRdData.allnonexistent := (selectedHartReg >= cfg.nComponents.U) & (~hamaskFull.reduce(_ | _))

    when(~DMSTATUSRdData.allnonexistent) { // if no existent harts selected, all other status is false
      DMSTATUSRdData.anyunavail := (io.i_debugUnavail.asUInt & hamaskFull.asUInt).asBools.reduce(_ | _)
      DMSTATUSRdData.anyhalted := ((~io.i_debugUnavail.asUInt & (haltedBitRegs)) & hamaskFull.asUInt).asBools
        .reduce(_ | _)
      DMSTATUSRdData.anyrunning := ((~io.i_debugUnavail.asUInt & ~(haltedBitRegs)) & hamaskFull.asUInt).asBools
        .reduce(_ | _)
      DMSTATUSRdData.anyhavereset := (haveResetBitRegs & hamaskFull.asUInt).asBools.reduce(_ | _)
      DMSTATUSRdData.anyresumeack := (resumeAcks & hamaskFull.asUInt).asBools.reduce(_ | _)
      when(~DMSTATUSRdData.anynonexistent) { // if one hart is nonexistent, no 'all' status is set
        DMSTATUSRdData.allunavail := (io.i_debugUnavail.asUInt | ~hamaskFull.asUInt).asBools.reduce(_ & _)
        DMSTATUSRdData.allhalted := ((~io.i_debugUnavail.asUInt & (haltedBitRegs)) | ~hamaskFull.asUInt).asBools
          .reduce(_ & _)
        DMSTATUSRdData.allrunning := ((~io.i_debugUnavail.asUInt & ~(haltedBitRegs)) | ~hamaskFull.asUInt).asBools
          .reduce(_ & _)
        DMSTATUSRdData.allhavereset := (haveResetBitRegs | ~hamaskFull.asUInt).asBools.reduce(_ & _)
        DMSTATUSRdData.allresumeack := (resumeAcks | ~hamaskFull.asUInt).asBools.reduce(_ & _)
      }
    }

    // TODO
    DMSTATUSRdData.confstrptrvalid := false.B
    DMSTATUSRdData.impebreak := (cfg.hasImplicitEbreak).B
  }

  when(~io.i_dmactive || ~i_dmAuthenticated) {
    haveResetBitRegs := 0.U
  }.otherwise {
    when(io.i_innerCtrl.fire && io.i_innerCtrl.bits.ackhavereset) {
      haveResetBitRegs := (haveResetBitRegs & (~(hamaskWrSel.asUInt))) | hartIsInResetSync.asUInt
    }.otherwise {
      haveResetBitRegs := haveResetBitRegs | hartIsInResetSync.asUInt
    }
  }

  // ----DMCS2 (Halt Groups)

  val DMCS2RdData = WireInit(0.U.asTypeOf(new DMCS2Fields()))
  val DMCS2WrData = WireInit(0.U.asTypeOf(new DMCS2Fields()))
  val hgselectWrEn = WireInit(false.B)
  val hgwriteWrEn = WireInit(false.B)
  val haltgroupWrEn = WireInit(false.B)
  val exttriggerWrEn = WireInit(false.B)
  val i_hgDebugInt = WireInit(VecInit(Seq.fill(cfg.nComponents) { false.B }))

  if (nHaltGroups > 0) { // TODO: async reset ensures triggers don't falsely fire during startup
    val hgBits = log2Up(nHaltGroups)
    // hgParticipate: Each entry indicates which hg that entity belongs to (1 to nHartGroups). 0 means no hg assigned.
    val hgParticipateHart = RegInit(VecInit(Seq.fill(cfg.nComponents)(0.U(hgBits.W))))
    val hgParticipateTrig = if (nExtTriggers > 0) RegInit(VecInit(Seq.fill(nExtTriggers)(0.U(hgBits.W)))) else Nil

    // assign group index to current seledcted harts
    for (component <- 0 until cfg.nComponents) {
      when(~io.i_dmactive || ~i_dmAuthenticated) {
        hgParticipateHart(component) := 0.U
      }.otherwise {
        when(
          haltgroupWrEn & DMCS2WrData.hgwrite & ~DMCS2WrData.hgselect &
            hamaskFull(component) & (DMCS2WrData.haltgroup <= nHaltGroups.U)
        ) {
          hgParticipateHart(component) := DMCS2WrData.haltgroup
        }
      }
    }
    DMCS2RdData.haltgroup := hgParticipateHart(if (cfg.nComponents == 1) 0.U(0.W) else selectedHartReg)

    if (nExtTriggers > 0) {
      val hgSelect = Reg(Bool())

      when(~io.i_dmactive || ~i_dmAuthenticated) {
        hgSelect := false.B
      }.otherwise {
        when(hgselectWrEn) {
          hgSelect := DMCS2WrData.hgselect
        }
      }

      // assign group index to trigger
      for (trigger <- 0 until nExtTriggers) {
        when(~io.i_dmactive || ~i_dmAuthenticated) {
          hgParticipateTrig(trigger) := 0.U
        }.otherwise {
          when(
            haltgroupWrEn & DMCS2WrData.hgwrite & DMCS2WrData.hgselect &
              (DMCS2WrData.exttrigger === trigger.U) & (DMCS2WrData.haltgroup <= nHaltGroups.U)
          ) {
            hgParticipateTrig(trigger) := DMCS2WrData.haltgroup
          }
        }
      }

      DMCS2RdData.hgselect := hgSelect
      when(hgSelect) {
        DMCS2RdData.haltgroup := hgParticipateTrig(0)
      }

      // If there is only 1 ext trigger, then the exttrigger field is fixed at 0
      // Otherwise, instantiate a register with only the number of bits required

      if (nExtTriggers > 1) {
        val trigBits = log2Up(nExtTriggers - 1)
        val hgExtTrigger = Reg(UInt(trigBits.W))
        when(~io.i_dmactive || ~i_dmAuthenticated) {
          hgExtTrigger := 0.U
        }.otherwise {
          when(exttriggerWrEn & (DMCS2WrData.exttrigger < nExtTriggers.U)) {
            hgExtTrigger := DMCS2WrData.exttrigger
          }
        }

        DMCS2RdData.exttrigger := hgExtTrigger
        when(hgSelect) {
          DMCS2RdData.haltgroup := hgParticipateTrig(hgExtTrigger.litValue.toInt)
        }
      }
    }

    // Halt group state machine
    //  IDLE:  Go to FIRED when any hart in this hg writes to HALTED while its HaltedBitRegs=0
    //                     or when any trigin assigned to this hg occurs
    //  FIRED: Back to IDLE when all harts in this hg have set their haltedBitRegs
    //                     and all trig out in this hg have been acknowledged

    val hgFired = RegInit(VecInit(Seq.fill(nHaltGroups + 1) { false.B }))
    val hgHartFiring = WireInit(VecInit(Seq.fill(nHaltGroups + 1) {
      false.B
    })) // which hg's are firing due to hart halting
    val hgTrigFiring = WireInit(VecInit(Seq.fill(nHaltGroups + 1) { false.B })) // which hg's are firing due to trig in
    val hgHartsAllHalted = WireInit(VecInit(Seq.fill(nHaltGroups + 1) {
      false.B
    })) // in which hg's have all harts halted
    val hgTrigsAllAcked = WireInit(VecInit(Seq.fill(nHaltGroups + 1) {
      true.B
    })) // in which hg's have all trigouts been acked

    io.extTrigger.foreach { extTrigger =>
      val extTriggerInReq = Wire(Vec(nExtTriggers, Bool()))
      val extTriggerOutAck = Wire(Vec(nExtTriggers, Bool()))
      extTriggerInReq := extTrigger.in.req
      extTriggerOutAck := extTrigger.out.ack
      val trigInReq = extTriggerInReq // TODO: ResetSynchronizerShiftReg
      val trigOutAck = extTriggerOutAck // TODO: ResetSynchronizerShiftReg
      for (hg <- 1 to nHaltGroups) {
        hgTrigFiring(hg) := (trigInReq.asUInt & ~(RegNext(trigInReq).asUInt) & VecInit(
          hgParticipateTrig.map(_ === hg.U)
        ).asUInt).asBools.reduce(_ | _)
        hgTrigsAllAcked(hg) := (trigOutAck.asUInt | VecInit(hgParticipateTrig.map(_ =/= hg.U)).asUInt).asBools
          .reduce(_ & _)
      }
      extTrigger.in.ack := trigInReq.asUInt
    }

    for (hg <- 1 to nHaltGroups) {
      hgHartFiring(hg) := hartHaltedWrEn & ~haltedBitRegs(hartHaltedId) & (hgParticipateHart(
        hartSelFuncs.hartIdToHartSel(hartHaltedId)
      ) === hg.U)
      hgHartsAllHalted(hg) := (haltedBitRegs.asUInt | VecInit(hgParticipateHart.map(_ =/= hg.U)).asUInt).asBools
        .reduce(_ & _)

      when(~io.i_dmactive || ~i_dmAuthenticated) {
        hgFired(hg) := false.B
      }.elsewhen(~hgFired(hg) & (hgHartFiring(hg) | hgTrigFiring(hg))) {
        hgFired(hg) := true.B
      }.elsewhen(hgFired(hg) & hgHartsAllHalted(hg) & hgTrigsAllAcked(hg)) {
        hgFired(hg) := false.B
      }
    }

    // For each hg that has fired, assert debug interrupt to each hart in that hg
    for (component <- 0 until cfg.nComponents) {
      i_hgDebugInt(component) := hgFired(hgParticipateHart(component))
    }

    // For each hg that has fired, assert trigger out for all external triggers in that hg
    io.extTrigger.foreach { extTrigger =>
      val extTriggerOutReq = RegInit(VecInit(Seq.fill(cfg.nExtTriggers) { false.B }))
      for (trig <- 0 until nExtTriggers) {
        extTriggerOutReq(trig) := hgFired(hgParticipateTrig(trig))
      }
      extTrigger.out.req := extTriggerOutReq.asUInt
    }
  }
  io.i_hgDebugInt := i_hgDebugInt.asUInt | hrDebugInt.asUInt

  // ----HALTSUM*
  val numHaltedStatus = ((cfg.nComponents - 1) / 32) + 1
  val haltedStatus = Wire(Vec(numHaltedStatus, Bits(32.W)))

  for (ii <- 0 until numHaltedStatus) {
    when(i_dmAuthenticated) {
      haltedStatus(ii) := haltedBitRegs >> (ii * 32)
    }.otherwise {
      haltedStatus(ii) := 0.U
    }
  }

  val haltedSummary = Cat(haltedStatus.map(_.orR).reverse)
  val HALTSUM1RdData = haltedSummary.asTypeOf(new HALTSUM1Fields())

  val selectedHaltedStatus = Mux((selectedHartReg >> 5) > numHaltedStatus.U, 0.U, haltedStatus(selectedHartReg >> 5))
  val HALTSUM0RdData = selectedHaltedStatus.asTypeOf(new HALTSUM0Fields())

  // Since we only support 1024 harts, we don't implement HALTSUM2 or HALTSUM3

  // ----ABSTRACTCS

  val ABSTRACTCSReset = WireInit(0.U.asTypeOf(new ABSTRACTCSFields()))
  ABSTRACTCSReset.datacount := cfg.nAbstractDataWords.U
  ABSTRACTCSReset.progbufsize := cfg.nProgramBufferWords.U

  val ABSTRACTCSReg = Reg(new ABSTRACTCSFields())
  val ABSTRACTCSWrData = WireInit(0.U.asTypeOf(new ABSTRACTCSFields()))
  val ABSTRACTCSRdData = WireInit(ABSTRACTCSReg)

  val ABSTRACTCSRdEn = WireInit(false.B)
  val ABSTRACTCSWrEnMaybe = WireInit(false.B)

  val ABSTRACTCSWrEnLegal = WireInit(false.B)
  val ABSTRACTCSWrEn = ABSTRACTCSWrEnMaybe && ABSTRACTCSWrEnLegal

  // multiple error types
  // find implement in the state machine part
  val errorBusy = WireInit(false.B)
  val errorException = WireInit(false.B)
  val errorUnsupported = WireInit(false.B)
  val errorHaltResume = WireInit(false.B)

  when(~io.i_dmactive || ~i_dmAuthenticated) {
    ABSTRACTCSReg := ABSTRACTCSReset
  }.otherwise {
    when(errorBusy) {
      ABSTRACTCSReg.cmderr := DebugAbstractCommandError.ErrBusy.id.U
    }.elsewhen(errorException) {
      ABSTRACTCSReg.cmderr := DebugAbstractCommandError.ErrException.id.U
    }.elsewhen(errorUnsupported) {
      ABSTRACTCSReg.cmderr := DebugAbstractCommandError.ErrNotSupported.id.U
    }.elsewhen(errorHaltResume) {
      ABSTRACTCSReg.cmderr := DebugAbstractCommandError.ErrHaltResume.id.U
    }.otherwise {
      // W1C
      when(ABSTRACTCSWrEn) {
        ABSTRACTCSReg.cmderr := ABSTRACTCSReg.cmderr & ~(ABSTRACTCSWrData.cmderr);
      }
    }
  }

  // For busy, see below state machine.
  val abstractCommandBusy = WireInit(true.B)
  ABSTRACTCSRdData.busy := abstractCommandBusy
  when(~i_dmAuthenticated) { // read value must be 0 when not authenticated
    ABSTRACTCSRdData.datacount := 0.U
    ABSTRACTCSRdData.progbufsize := 0.U
  }

  // ---- ABSTRACTAUTO
  // It is a mask indicating whether datai/probufi have the autoexcution permisson
  // this part aims to produce 3 wires : autoexecData,autoexecProg,autoexec
  // first two specify which reg supports autoexec
  // autoexec is a control signal, meaning there is at least one enabled autoexec reg
  // when autoexec is set, generate instructions using COMMAND register

  val ABSTRACTAUTOReset = WireInit(0.U.asTypeOf(new ABSTRACTAUTOFields()))
  val ABSTRACTAUTOReg = Reg(new ABSTRACTAUTOFields())
  val ABSTRACTAUTOWrData = WireInit(0.U.asTypeOf(new ABSTRACTAUTOFields()))
  val ABSTRACTAUTORdData = WireInit(ABSTRACTAUTOReg)

  val ABSTRACTAUTORdEn = WireInit(false.B)
  val autoexecdataWrEnMaybe = WireInit(false.B)
  val autoexecprogbufWrEnMaybe = WireInit(false.B)

  val ABSTRACTAUTOWrEnLegal = WireInit(false.B)

  when(~io.i_dmactive || ~i_dmAuthenticated) {
    ABSTRACTAUTOReg := ABSTRACTAUTOReset
  }.otherwise {
    when(autoexecprogbufWrEnMaybe && ABSTRACTAUTOWrEnLegal) {
      ABSTRACTAUTOReg.autoexecprogbuf := ABSTRACTAUTOWrData.autoexecprogbuf & ((1 << cfg.nProgramBufferWords) - 1).U
    }
    when(autoexecdataWrEnMaybe && ABSTRACTAUTOWrEnLegal) {
      ABSTRACTAUTOReg.autoexecdata := ABSTRACTAUTOWrData.autoexecdata & ((1 << cfg.nAbstractDataWords) - 1).U
    }
  }

  // Abstract Data access vector(byte-addressable)
  val dmiAbstractDataAccessVec = WireInit(VecInit(Seq.fill(cfg.nAbstractDataWords * 4) { false.B }))
  dmiAbstractDataAccessVec := (dmiAbstractDataWrEnMaybe.zip(dmiAbstractDataRdEn)).map { case (r, w) => r | w }
  // Program Buffer access vector(byte-addressable)
  val dmiProgramBufferAccessVec = WireInit(VecInit(Seq.fill(cfg.nProgramBufferWords * 4) { false.B }))
  dmiProgramBufferAccessVec := (dmiProgramBufferWrEnMaybe.zip(dmiProgramBufferRdEn)).map { case (r, w) => r | w }
  // at least one word access
  val dmiAbstractDataAccess = dmiAbstractDataAccessVec.reduce(_ || _)
  val dmiProgramBufferAccess = dmiProgramBufferAccessVec.reduce(_ || _)

  // This will take the shorter of the lists, which is what we want.
  val autoexecData = WireInit(VecInit(Seq.fill(cfg.nAbstractDataWords) { false.B }))
  val autoexecProg = WireInit(VecInit(Seq.fill(cfg.nProgramBufferWords) { false.B }))
  (autoexecData.zip(ABSTRACTAUTOReg.autoexecdata.asBools)).zipWithIndex.foreach { case (t, i) =>
    t._1 := dmiAbstractDataAccessVec(i * 4) && t._2
  }
  (autoexecProg.zip(ABSTRACTAUTOReg.autoexecprogbuf.asBools)).zipWithIndex.foreach { case (t, i) =>
    t._1 := dmiProgramBufferAccessVec(i * 4) && t._2
  }

  val autoexec = autoexecData.reduce(_ || _) || autoexecProg.reduce(_ || _)

  // ---- COMMAND

  val COMMANDReset = WireInit(0.U.asTypeOf(new COMMANDFields()))
  val COMMANDReg = Reg(new COMMANDFields())

  val COMMANDWrDataVal = WireInit(0.U(32.W))
  val COMMANDWrData = WireInit(COMMANDWrDataVal.asTypeOf(new COMMANDFields()))
  val COMMANDWrEnMaybe = WireInit(false.B)
  val COMMANDWrEnLegal = WireInit(false.B)
  val COMMANDRdEn = WireInit(false.B)

  val COMMANDWrEn = COMMANDWrEnMaybe && COMMANDWrEnLegal
  val COMMANDRdData = COMMANDReg

  when(~io.i_dmactive || ~i_dmAuthenticated) {
    COMMANDReg := COMMANDReset
  }.otherwise {
    when(COMMANDWrEn) {
      COMMANDReg := COMMANDWrData
    }
  }

  // --- Abstract Data

  // These are byte addressible, s.t. the Processor can use
  // byte-addressible instructions to store to them.
  val abstractDataMem = Reg(Vec(cfg.nAbstractDataWords * 4, UInt(8.W)))
  val abstractDataNxt = WireInit(abstractDataMem)

  // --- Program Buffer

  // byte-addressible mem
  val programBufferMem = Reg(Vec(cfg.nProgramBufferWords * 4, UInt(8.W)))
  val programBufferNxt = WireInit(programBufferMem)

  // --------------------------------------------------------------
  // These bits are implementation-specific bits set
  // by harts executing code.
  // --------------------------------------------------------------

  // Run control logic
  when(~io.i_dmactive || ~i_dmAuthenticated) {
    haltedBitRegs := 0.U
    resumeReqRegs := 0.U
  }.otherwise {
    // remove those harts in reset
    resumeReqRegs := resumeReqRegs & ~(hartIsInResetSync.asUInt)

    val hartHaltedIdIndex = UIntToOH(hartSelFuncs.hartIdToHartSel(hartHaltedId))
    val hartResumingIdIndex = UIntToOH(hartSelFuncs.hartIdToHartSel(hartResumingId))
    val hartselIndex = UIntToOH(io.i_innerCtrl.bits.hartsel)
    when(hartHaltedWrEn) {
      // add those harts halting and remove those in reset
      haltedBitRegs := (haltedBitRegs | hartHaltedIdIndex) & ~(hartIsInResetSync.asUInt)
    }.elsewhen(hartResumingWrEn) {
      // remove those harts in reset and those in resume
      haltedBitRegs := (haltedBitRegs & ~(hartResumingIdIndex)) & ~(hartIsInResetSync.asUInt)
    }.otherwise {
      // remove those harts in reset
      haltedBitRegs := haltedBitRegs & ~(hartIsInResetSync.asUInt)
    }

    when(hartResumingWrEn) {
      // remove those harts in resume and those in reset
      resumeReqRegs := (resumeReqRegs & ~(hartResumingIdIndex)) & ~(hartIsInResetSync.asUInt)
    }
    when(resumereq) {
      // set all sleceted harts to resumeReq, remove those in reset
      resumeReqRegs := (resumeReqRegs | hamaskWrSel.asUInt) & ~(hartIsInResetSync.asUInt)
    }

  }

  when(resumereq) {
    // next cycle resumeAcls will be the negation of next cycle resumeReqRegs
    resumeAcks := (~resumeReqRegs & ~(hamaskWrSel.asUInt))
  }.otherwise {
    resumeAcks := ~resumeReqRegs
  }

  // ---- AUTHDATA
  val authRdEnMaybe = WireInit(false.B)
  val authWrEnMaybe = WireInit(false.B)
  io.i_auth.map { a =>
    a.dmactive := io.i_dmactive
    a.dmAuthRead := authRdEnMaybe & ~a.dmAuthBusy
    a.dmAuthWrite := authWrEnMaybe & ~a.dmAuthBusy
  }

  val dmstatusRegFields = RegFieldGroup(
    "dmi_dmstatus",
    Some("debug module status register"),
    Seq(
      RegField.r(4, DMSTATUSRdData.version, RegFieldDesc("version", "version", reset = Some(2))),
      RegField
        .r(1, DMSTATUSRdData.confstrptrvalid, RegFieldDesc("confstrptrvalid", "confstrptrvalid", reset = Some(0))),
      RegField
        .r(1, DMSTATUSRdData.hasresethaltreq, RegFieldDesc("hasresethaltreq", "hasresethaltreq", reset = Some(1))),
      RegField.r(1, DMSTATUSRdData.authbusy, RegFieldDesc("authbusy", "authbusy", reset = Some(0))),
      RegField.r(1, DMSTATUSRdData.authenticated, RegFieldDesc("authenticated", "authenticated", reset = Some(1))),
      RegField.r(1, DMSTATUSRdData.anyhalted, RegFieldDesc("anyhalted", "anyhalted", reset = Some(0))),
      RegField.r(1, DMSTATUSRdData.allhalted, RegFieldDesc("allhalted", "allhalted", reset = Some(0))),
      RegField.r(1, DMSTATUSRdData.anyrunning, RegFieldDesc("anyrunning", "anyrunning", reset = Some(1))),
      RegField.r(1, DMSTATUSRdData.allrunning, RegFieldDesc("allrunning", "allrunning", reset = Some(1))),
      RegField.r(1, DMSTATUSRdData.anyunavail, RegFieldDesc("anyunavail", "anyunavail", reset = Some(0))),
      RegField.r(1, DMSTATUSRdData.allunavail, RegFieldDesc("allunavail", "allunavail", reset = Some(0))),
      RegField.r(1, DMSTATUSRdData.anynonexistent, RegFieldDesc("anynonexistent", "anynonexistent", reset = Some(0))),
      RegField.r(1, DMSTATUSRdData.allnonexistent, RegFieldDesc("allnonexistent", "allnonexistent", reset = Some(0))),
      RegField.r(1, DMSTATUSRdData.anyresumeack, RegFieldDesc("anyresumeack", "anyresumeack", reset = Some(1))),
      RegField.r(1, DMSTATUSRdData.allresumeack, RegFieldDesc("allresumeack", "allresumeack", reset = Some(1))),
      RegField.r(1, DMSTATUSRdData.anyhavereset, RegFieldDesc("anyhavereset", "anyhavereset", reset = Some(0))),
      RegField.r(1, DMSTATUSRdData.allhavereset, RegFieldDesc("allhavereset", "allhavereset", reset = Some(0))),
      RegField(2),
      RegField.r(
        1,
        DMSTATUSRdData.impebreak,
        RegFieldDesc("impebreak", "impebreak", reset = Some(if (cfg.hasImplicitEbreak) 1 else 0))
      )
    )
  )

  val dmcs2RegFields = RegFieldGroup(
    "dmi_dmcs2",
    Some("debug module control/status register 2"),
    Seq(
      WNotifyVal(
        1,
        DMCS2RdData.hgselect,
        DMCS2WrData.hgselect,
        hgselectWrEn,
        RegFieldDesc("hgselect", "select halt groups or external triggers", reset = Some(0), volatile = true)
      ),
      WNotifyVal(
        1,
        0.U,
        DMCS2WrData.hgwrite,
        hgwriteWrEn,
        RegFieldDesc("hgwrite", "write 1 to change halt groups", reset = None, access = RegFieldAccessType.W)
      ),
      WNotifyVal(
        5,
        DMCS2RdData.haltgroup,
        DMCS2WrData.haltgroup,
        haltgroupWrEn,
        RegFieldDesc("haltgroup", "halt group", reset = Some(0), volatile = true)
      ),
      if (nExtTriggers > 1)
        WNotifyVal(
          4,
          DMCS2RdData.exttrigger,
          DMCS2WrData.exttrigger,
          exttriggerWrEn,
          RegFieldDesc("exttrigger", "external trigger select", reset = Some(0), volatile = true)
        )
      else RegField(4)
    )
  )

  val abstractcsRegFields = RegFieldGroup(
    "dmi_abstractcs",
    Some("abstract command control/status"),
    Seq(
      RegField.r(
        4,
        ABSTRACTCSRdData.datacount,
        RegFieldDesc("datacount", "number of DATA registers", reset = Some(cfg.nAbstractDataWords))
      ),
      RegField(4),
      WNotifyVal(
        3,
        ABSTRACTCSRdData.cmderr,
        ABSTRACTCSWrData.cmderr,
        ABSTRACTCSWrEnMaybe,
        RegFieldDesc("cmderr", "command error", reset = Some(0), wrType = Some(RegFieldWrType.ONE_TO_CLEAR))
      ),
      RegField(1),
      RegField.r(1, ABSTRACTCSRdData.busy, RegFieldDesc("busy", "busy", reset = Some(0))),
      RegField(11),
      RegField.r(
        5,
        ABSTRACTCSRdData.progbufsize,
        RegFieldDesc("progbufsize", "number of PROGBUF registers", reset = Some(cfg.nProgramBufferWords))
      )
    )
  )

  // val (sbcsFields, sbAddrFields, sbDataFields):
  // (Seq[RegField], Seq[Seq[RegField]], Seq[Seq[RegField]]) = sb2tlOpt.map{ sb2tl =>
  //   SystemBusAccessModule(sb2tl, io.i_dmactive, i_dmAuthenticated)(p)
  // }.getOrElse((Seq.empty[RegField], Seq.fill[Seq[RegField]](4)(Seq.empty[RegField]), Seq.fill[Seq[RegField]](4)(Seq.empty[RegField])))

  // --------------------------------------------------------------
  // Program Buffer Access (DMI ... System Bus can override)
  // --------------------------------------------------------------

  // val omRegMap = i_dmiNode.regmap(
  regmap(
    io.i_dmiNode.viewAs[AXI4RWIrrevocable],
    0,
    false,
    (DMI_DMSTATUS << 2) -> dmstatusRegFields,
    // TODO (DMI_CFGSTRADDR0 << 2) -> cfgStrAddrFields,
    (DMI_DMCS2 << 2) -> (if (nHaltGroups > 0) dmcs2RegFields else Nil),
    (DMI_HALTSUM0 << 2) -> RegFieldGroup(
      "dmi_haltsum0",
      Some("Halt Summary 0"),
      Seq(RegField.r(32, HALTSUM0RdData.asUInt, RegFieldDesc("dmi_haltsum0", "halt summary 0")))
    ),
    (DMI_HALTSUM1 << 2) -> RegFieldGroup(
      "dmi_haltsum1",
      Some("Halt Summary 1"),
      Seq(RegField.r(32, HALTSUM1RdData.asUInt, RegFieldDesc("dmi_haltsum1", "halt summary 1")))
    ),
    (DMI_ABSTRACTCS << 2) -> abstractcsRegFields,
    (DMI_ABSTRACTAUTO << 2) -> RegFieldGroup(
      "dmi_abstractauto",
      Some("abstract command autoexec"),
      Seq(
        WNotifyVal(
          cfg.nAbstractDataWords,
          ABSTRACTAUTORdData.autoexecdata,
          ABSTRACTAUTOWrData.autoexecdata,
          autoexecdataWrEnMaybe,
          RegFieldDesc("autoexecdata", "abstract command data autoexec", reset = Some(0))
        ),
        RegField(16 - cfg.nAbstractDataWords),
        WNotifyVal(
          cfg.nProgramBufferWords,
          ABSTRACTAUTORdData.autoexecprogbuf,
          ABSTRACTAUTOWrData.autoexecprogbuf,
          autoexecprogbufWrEnMaybe,
          RegFieldDesc("autoexecprogbuf", "abstract command progbuf autoexec", reset = Some(0))
        )
      )
    ),
    (DMI_COMMAND << 2) -> RegFieldGroup(
      "dmi_command",
      Some("Abstract Command Register"),
      Seq(
        RWNotify(
          32,
          COMMANDRdData.asUInt,
          COMMANDWrDataVal,
          COMMANDRdEn,
          COMMANDWrEnMaybe,
          Some(RegFieldDesc("dmi_command", "abstract command register", reset = Some(0), volatile = true))
        )
      )
    ),
    (DMI_DATA0 << 2) -> RegFieldGroup(
      "dmi_data",
      Some("abstract command data registers"),
      abstractDataMem.zipWithIndex.map { case (x, i) =>
        RWNotify(
          8,
          Mux(i_dmAuthenticated, x, 0.U),
          abstractDataNxt(i),
          dmiAbstractDataRdEn(i),
          dmiAbstractDataWrEnMaybe(i),
          Some(RegFieldDesc(s"dmi_data_$i", s"abstract command data register $i", reset = Some(0), volatile = true))
        )
      },
      false
    ),
    (DMI_PROGBUF0 << 2) -> RegFieldGroup(
      "dmi_progbuf",
      Some("abstract command progbuf registers"),
      programBufferMem.zipWithIndex.map { case (x, i) =>
        RWNotify(
          8,
          Mux(i_dmAuthenticated, x, 0.U),
          programBufferNxt(i),
          dmiProgramBufferRdEn(i),
          dmiProgramBufferWrEnMaybe(i),
          Some(RegFieldDesc(s"dmi_progbuf_$i", s"abstract command progbuf register $i", reset = Some(0)))
        )
      },
      false
    ),
    (DMI_AUTHDATA << 2) -> (if (cfg.hasAuthentication)
                              RegFieldGroup(
                                "dmi_authdata",
                                Some("authentication data exchange register"),
                                Seq(
                                  RWNotify(
                                    32,
                                    io.i_auth.get.dmAuthRdata,
                                    io.i_auth.get.dmAuthWdata,
                                    authRdEnMaybe,
                                    authWrEnMaybe,
                                    Some(RegFieldDesc("authdata", "authentication data exchange", volatile = true))
                                  )
                                )
                              )
                            else Nil)
    // (DMI_SBCS       << 2) -> sbcsFields,
    // (DMI_SBDATA0    << 2) -> sbDataFields(0),
    // (DMI_SBDATA1    << 2) -> sbDataFields(1),
    // (DMI_SBDATA2    << 2) -> sbDataFields(2),
    // (DMI_SBDATA3    << 2) -> sbDataFields(3),
    // (DMI_SBADDRESS0 << 2) -> sbAddrFields(0),
    // (DMI_SBADDRESS1 << 2) -> sbAddrFields(1),
    // (DMI_SBADDRESS2 << 2) -> sbAddrFields(2),
    // (DMI_SBADDRESS3 << 2) -> sbAddrFields(3)
  )

  // Abstract data mem is written by both the tile link interface and DMI...
  abstractDataMem.zipWithIndex.foreach { case (x, i) =>
    when(i_dmAuthenticated && dmiAbstractDataWrEnMaybe(i) && dmiAbstractDataAccessLegal) {
      x := abstractDataNxt(i)
    }
  }
  // ... and also by custom register read (if implemented)
  // val (customs, customParams) = customNode.in.unzip
  // val needCustom = (customs.size > 0) && (customParams.head.addrs.size > 0)
  // def getNeedCustom = () => needCustom

  // if (needCustom) {
  //   val (custom, customP) = customNode.in.head
  //   require(customP.width % 8 == 0, s"Debug Custom width must be divisible by 8, not ${customP.width}")
  //   val custom_data = custom.data.asBools
  //   val custom_bytes =  Seq.tabulate(customP.width/8){i => custom_data.slice(i*8, (i+1)*8).asUInt}
  //   when (custom.ready && custom.valid) {
  //     (abstractDataMem zip custom_bytes).zipWithIndex.foreach {case ((a, b), i) =>
  //       a := b
  //     }
  //   }
  // }

  programBufferMem.zipWithIndex.foreach { case (x, i) =>
    when(i_dmAuthenticated && dmiProgramBufferWrEnMaybe(i) && dmiProgramBufferAccessLegal) {
      x := programBufferNxt(i)
    }
  }

  // --------------------------------------------------------------
  // "Variable" ROM Generation
  // --------------------------------------------------------------

  val goReg = Reg(Bool())
  val goAbstract = WireInit(false.B)
  val goCustom = WireInit(false.B)
  val jalAbstract = WireInit(Instructions.JAL.value.U.asTypeOf(new GeneratedUJ()))
  jalAbstract.setImm(ABSTRACT(cfg) - WHERETO)

  when(~io.i_dmactive) {
    goReg := false.B
  }.otherwise {
    when(goAbstract) {
      goReg := true.B
    }.elsewhen(hartGoingWrEn) {
      assert(hartGoingId === 0.U, "Unexpected 'GOING' hart.") // Chisel3 #540 %x, expected %x", hartGoingId, 0.U)
      goReg := false.B
    }
  }

  class flagBundle extends Bundle {
    val reserved = UInt(6.W)
    val resume = Bool()
    val go = Bool()
  }

  val flags = WireInit(VecInit(Seq.fill(1 << selectedHartReg.getWidth) { 0.U.asTypeOf(new flagBundle()) }))
  assert(
    (hartSelFuncs.hartSelToHartId(selectedHartReg) < flags.size.U),
    s"HartSel to HartId Mapping is illegal for this Debug Implementation, because HartID must be < ${flags.size} for it to work."
  )
  flags(hartSelFuncs.hartSelToHartId(selectedHartReg)).go := goReg

  for (component <- 0 until cfg.nComponents) {
    val componentSel = WireInit(component.U)
    flags(hartSelFuncs.hartSelToHartId(componentSel)).resume := resumeReqRegs(component)
  }

  // ----------------------------
  // Abstract Command Decoding & Generation
  // ----------------------------

  val accessRegisterCommandWr = WireInit(COMMANDWrData.asUInt.asTypeOf(new ACCESS_REGISTERFields()))

  /** real COMMAND */
  val accessRegisterCommandReg = WireInit(COMMANDReg.asUInt.asTypeOf(new ACCESS_REGISTERFields()))

  // TODO: Quick Access

  class GeneratedI extends Bundle {
    val imm = UInt(12.W)
    val rs1 = UInt(5.W)
    val funct3 = UInt(3.W)
    val rd = UInt(5.W)
    val opcode = UInt(7.W)
  }

  class GeneratedS extends Bundle {
    val immhi = UInt(7.W)
    val rs2 = UInt(5.W)
    val rs1 = UInt(5.W)
    val funct3 = UInt(3.W)
    val immlo = UInt(5.W)
    val opcode = UInt(7.W)
  }

  class GeneratedCSR extends Bundle {
    val imm = UInt(12.W)
    val rs1 = UInt(5.W)
    val funct3 = UInt(3.W)
    val rd = UInt(5.W)
    val opcode = UInt(7.W)
  }

  class GeneratedUJ extends Bundle {
    val imm3 = UInt(1.W)
    val imm0 = UInt(10.W)
    val imm1 = UInt(1.W)
    val imm2 = UInt(8.W)
    val rd = UInt(5.W)
    val opcode = UInt(7.W)

    def setImm(imm: Int): Unit = {
      // TODO: Check bounds of imm.

      require(imm % 2 == 0, "Immediate must be even for UJ encoding.")
      val immWire = WireInit(imm.S(21.W))
      val immBits = WireInit(VecInit(immWire.asBools))

      imm0 := VecInit(immBits.slice(1, 1 + 10)).asUInt
      imm1 := VecInit(immBits.slice(11, 11 + 11)).asUInt
      imm2 := VecInit(immBits.slice(12, 12 + 8)).asUInt
      imm3 := VecInit(immBits.slice(20, 20 + 1)).asUInt
    }
  }

  require(
    (cfg.atzero && cfg.nAbstractInstructions == 2) || (!cfg.atzero && cfg.nAbstractInstructions == 5),
    "Mismatch between DMParameter atzero and nAbstractInstructions"
  )
  val abstractGeneratedMem = Reg(Vec(cfg.nAbstractInstructions, (UInt(32.W))))

  def abstractGeneratedI(cfg: DMParameter): UInt = {
    val inst = Wire(new GeneratedI())
    val offset = if (cfg.atzero) DATA else (DATA - 0x800) & 0xfff
    val base = if (cfg.atzero) 0.U else Mux(accessRegisterCommandReg.regno(0), 8.U, 9.U)
    inst.opcode := (Instructions.LW.value.U.asTypeOf(new GeneratedI())).opcode
    inst.rd := (accessRegisterCommandReg.regno & 0x1f.U)
    inst.funct3 := accessRegisterCommandReg.size
    inst.rs1 := base
    inst.imm := offset.U
    inst.asUInt
  }

  def abstractGeneratedS(cfg: DMParameter): UInt = {
    val inst = Wire(new GeneratedS())
    val offset = if (cfg.atzero) DATA else (DATA - 0x800) & 0xfff
    val base = if (cfg.atzero) 0.U else Mux(accessRegisterCommandReg.regno(0), 8.U, 9.U)
    inst.opcode := (Instructions.SW.value.U.asTypeOf(new GeneratedS())).opcode
    inst.immlo := (offset & 0x1f).U
    inst.funct3 := accessRegisterCommandReg.size
    inst.rs1 := base
    inst.rs2 := (accessRegisterCommandReg.regno & 0x1f.U)
    inst.immhi := (offset >> 5).U
    inst.asUInt
  }

  def abstractGeneratedCSR: UInt = {
    val inst = Wire(new GeneratedCSR())
    val base = Mux(accessRegisterCommandReg.regno(0), 8.U, 9.U) // use s0 as base for odd regs, s1 as base for even regs
    inst := (Instructions.CSRRW.value.U.asTypeOf(new GeneratedCSR()))
    inst.imm := CSRs.dscratch1.U
    inst.rs1 := base
    inst.rd := base
    inst.asUInt
  }

  val nop = Wire(new GeneratedI())
  nop := Instructions.ADDI.value.U.asTypeOf(new GeneratedI())
  nop.rd := 0.U
  nop.rs1 := 0.U
  nop.imm := 0.U

  val isa = Wire(new GeneratedI())
  isa := Instructions.ADDIW.value.U.asTypeOf(new GeneratedI())
  isa.rd := 0.U
  isa.rs1 := 0.U
  isa.imm := 0.U

  when(goAbstract) {
    if (cfg.nAbstractInstructions == 2) {
      // ABSTRACT(0): Transfer: LW or SW, else NOP
      // ABSTRACT(1): Postexec: NOP       else EBREAK
      abstractGeneratedMem(0) := Mux(
        accessRegisterCommandReg.transfer,
        Mux(accessRegisterCommandReg.write, abstractGeneratedI(cfg), abstractGeneratedS(cfg)),
        nop.asUInt
      )
      abstractGeneratedMem(1) := Mux(accessRegisterCommandReg.postexec, nop.asUInt, Instructions.EBREAK.value.U)
    } else {
      // Entry: All regs in GPRs, dscratch1=offset 0x800 in DM
      // ABSTRACT(0): CheckISA: ADDW or NOP (exception here if size=3 and not RV64)
      // ABSTRACT(1):           CSRRW s1,dscratch1,s1 or CSRRW s0,dscratch1,s0
      // ABSTRACT(2): Transfer: LW, SW, LD, SD else NOP
      // ABSTRACT(3):           CSRRW s1,dscratch1,s1 or CSRRW s0,dscratch1,s0
      // ABSTRACT(4): Postexec: NOP else EBREAK
      abstractGeneratedMem(0) := Mux(
        accessRegisterCommandReg.transfer && accessRegisterCommandReg.size =/= 2.U,
        isa.asUInt,
        nop.asUInt
      )
      abstractGeneratedMem(1) := abstractGeneratedCSR
      abstractGeneratedMem(2) := Mux(
        accessRegisterCommandReg.transfer,
        Mux(accessRegisterCommandReg.write, abstractGeneratedI(cfg), abstractGeneratedS(cfg)),
        nop.asUInt
      )
      abstractGeneratedMem(3) := abstractGeneratedCSR
      abstractGeneratedMem(4) := Mux(accessRegisterCommandReg.postexec, nop.asUInt, Instructions.EBREAK.value.U)
    }
  }

  // --------------------------------------------------------------
  // Drive Custom Access
  // --------------------------------------------------------------
  // if (needCustom) {
  //   val (custom, customP) = customNode.in.head
  //   custom.addr  := accessRegisterCommandReg.regno
  //   custom.valid := goCustom
  // }
  // --------------------------------------------------------------
  // Hart Bus Access
  // --------------------------------------------------------------

  regmap(
    io.i_tlNode.viewAs[AXI4RWIrrevocable],
    0,
    false,
    // This memory is writable.
    HALTED -> Seq(
      WNotifyWire(
        sbIdWidth,
        hartHaltedId,
        hartHaltedWrEn,
        "debug_hart_halted",
        "Debug ROM Causes hart to write its hartID here when it is in Debug Mode."
      )
    ),
    GOING -> Seq(
      WNotifyWire(
        sbIdWidth,
        hartGoingId,
        hartGoingWrEn,
        "debug_hart_going",
        "Debug ROM causes hart to write 0 here when it begins executing Debug Mode instructions."
      )
    ),
    RESUMING -> Seq(
      WNotifyWire(
        sbIdWidth,
        hartResumingId,
        hartResumingWrEn,
        "debug_hart_resuming",
        "Debug ROM causes hart to write its hartID here when it leaves Debug Mode."
      )
    ),
    EXCEPTION -> Seq(
      WNotifyWire(
        sbIdWidth,
        hartExceptionId,
        hartExceptionWrEn,
        "debug_hart_exception",
        "Debug ROM causes hart to write 0 here if it gets an exception in Debug Mode."
      )
    ),
    DATA -> RegFieldGroup(
      "debug_data",
      Some("Data used to communicate with Debug Module"),
      abstractDataMem.zipWithIndex.map { case (x, i) => RegField(8, x, RegFieldDesc(s"debug_data_$i", "")) }
    ),
    PROGBUF(cfg) -> RegFieldGroup(
      "debug_progbuf",
      Some("Program buffer used to communicate with Debug Module"),
      programBufferMem.zipWithIndex.map { case (x, i) => RegField(8, x, RegFieldDesc(s"debug_progbuf_$i", "")) }
    ),

    // These sections are read-only.
    IMPEBREAK(cfg) -> {
      if (cfg.hasImplicitEbreak)
        Seq(
          RegField.r(
            32,
            Instructions.EBREAK.value.U,
            RegFieldDesc("debug_impebreak", "Debug Implicit EBREAK", reset = Some(Instructions.EBREAK.value))
          )
        )
      else Nil
    },
    WHERETO -> Seq(
      RegField.r(
        32,
        jalAbstract.asUInt,
        RegFieldDesc(
          "debug_whereto",
          "Instruction filled in by Debug Module to control hart in Debug Mode",
          volatile = true
        )
      )
    ),
    ABSTRACT(cfg) -> RegFieldGroup(
      "debug_abstract",
      Some("Instructions generated by Debug Module"),
      abstractGeneratedMem.zipWithIndex.map { case (x, i) =>
        RegField.r(32, x, RegFieldDesc(s"debug_abstract_$i", "", volatile = true))
      }
    ),
    FLAGS -> RegFieldGroup(
      "debug_flags",
      Some("Memory region used to control hart going/resuming in Debug Mode"),
      if (cfg.nComponents == 1) {
        Seq.tabulate(1024) { i => RegField.r(8, flags(0).asUInt, RegFieldDesc(s"debug_flags_$i", "", volatile = true)) }
      } else {
        flags.zipWithIndex.map { case (x, i) =>
          RegField.r(8, x.asUInt, RegFieldDesc(s"debug_flags_$i", "", volatile = true))
        }
      }
    ),
    ROMBASE -> RegFieldGroup(
      "debug_rom",
      Some("Debug ROM"),
      (if (cfg.atzero) DebugRomContents() else DebugRomNonzeroContents()).zipWithIndex.map { case (x, i) =>
        RegField.r(8, (x & 0xff).U(8.W), RegFieldDesc(s"debug_rom_$i", "", reset = Some(x)))
      }
    )
  )

  // Override System Bus accesses with dmactive reset.
  when(~io.i_dmactive) {
    abstractDataMem.foreach { x => x := 0.U }
    programBufferMem.foreach { x => x := 0.U }
  }

  // --------------------------------------------------------------
  // Abstract Command State Machine
  // --------------------------------------------------------------

  object CtrlState extends scala.Enumeration {
    type CtrlState = Value
    val Waiting, CheckGenerate, Exec, Custom = Value

    def apply(t: Value): UInt = {
      t.id.U(log2Up(values.size).W)
    }
  }
  import CtrlState._

  // This is not an initialization!
  val ctrlStateReg = Reg(chiselTypeOf(CtrlState(Waiting)))

  val hartHalted = haltedBitRegs(if (cfg.nComponents == 1) 0.U(0.W) else selectedHartReg)
  val ctrlStateNxt = WireInit(ctrlStateReg)

  // ------------------------
  // DMI Register Control and Status

  abstractCommandBusy := (ctrlStateReg =/= CtrlState(Waiting))

  ABSTRACTCSWrEnLegal := (ctrlStateReg === CtrlState(Waiting))
  COMMANDWrEnLegal := (ctrlStateReg === CtrlState(Waiting))
  ABSTRACTAUTOWrEnLegal := (ctrlStateReg === CtrlState(Waiting))
  dmiAbstractDataAccessLegal := (ctrlStateReg === CtrlState(Waiting))
  dmiProgramBufferAccessLegal := (ctrlStateReg === CtrlState(Waiting))

  errorBusy := (ABSTRACTCSWrEnMaybe && ~ABSTRACTCSWrEnLegal) ||
    (autoexecdataWrEnMaybe && ~ABSTRACTAUTOWrEnLegal) ||
    (autoexecprogbufWrEnMaybe && ~ABSTRACTAUTOWrEnLegal) ||
    (COMMANDWrEnMaybe && ~COMMANDWrEnLegal) ||
    (dmiAbstractDataAccess && ~dmiAbstractDataAccessLegal) ||
    (dmiProgramBufferAccess && ~dmiProgramBufferAccessLegal)

  // TODO: Maybe Quick Access
  val commandWrIsAccessRegister = (COMMANDWrData.cmdtype === DebugAbstractCommandType.AccessRegister.id.U)
  val commandRegIsAccessRegister = (COMMANDReg.cmdtype === DebugAbstractCommandType.AccessRegister.id.U)

  val commandWrIsUnsupported = COMMANDWrEn && !commandWrIsAccessRegister

  val commandRegIsUnsupported = WireInit(true.B)
  val commandRegBadHaltResume = WireInit(false.B)

  // We only support abstract commands for GPRs and any custom registers, if specified.
  val accessRegIsLegalSize = (accessRegisterCommandReg.size === 2.U) || (accessRegisterCommandReg.size === 3.U)
  val accessRegIsGPR =
    (accessRegisterCommandReg.regno >= 0x1000.U && accessRegisterCommandReg.regno <= 0x101f.U) && accessRegIsLegalSize
  val accessRegIsCustom = false.B // TODO
  // val accessRegIsCustom = if (needCustom) {
  //   val (custom, customP) = customNode.in.head
  //   customP.addrs.foldLeft(false.B){
  //     (result, current) => result || (current.U === accessRegisterCommandReg.regno)}
  // } else false.B

  when(commandRegIsAccessRegister) {
    when(accessRegIsCustom && accessRegisterCommandReg.transfer && accessRegisterCommandReg.write === false.B) {
      commandRegIsUnsupported := false.B
    }.elsewhen(!accessRegisterCommandReg.transfer || accessRegIsGPR) {
      commandRegIsUnsupported := false.B
      commandRegBadHaltResume := ~hartHalted
    }
  }

  val wrAccessRegisterCommand = COMMANDWrEn && commandWrIsAccessRegister && (ABSTRACTCSReg.cmderr === 0.U)
  val regAccessRegisterCommand = autoexec && commandRegIsAccessRegister && (ABSTRACTCSReg.cmderr === 0.U)

  // ------------------------
  // Variable ROM STATE MACHINE
  // -----------------------

  when(ctrlStateReg === CtrlState(Waiting)) {
    when(wrAccessRegisterCommand || regAccessRegisterCommand) {
      ctrlStateNxt := CtrlState(CheckGenerate)
    }.elsewhen(commandWrIsUnsupported) { // These checks are really on the command type.
      errorUnsupported := true.B
    }.elsewhen(autoexec && commandRegIsUnsupported) {
      errorUnsupported := true.B
    }
  }.elsewhen(ctrlStateReg === CtrlState(CheckGenerate)) {

    // We use this state to ensure that the COMMAND has been
    // registered by the time that we need to use it, to avoid
    // generating it directly from the COMMANDWrData.
    // This 'commandRegIsUnsupported' is really just checking the
    // AccessRegisterCommand parameters (regno)
    when(commandRegIsUnsupported) {
      errorUnsupported := true.B
      ctrlStateNxt := CtrlState(Waiting)
    }.elsewhen(commandRegBadHaltResume) {
      errorHaltResume := true.B
      ctrlStateNxt := CtrlState(Waiting)
    }.otherwise {
      when(accessRegIsCustom) {
        ctrlStateNxt := CtrlState(Custom)
      }.otherwise {
        ctrlStateNxt := CtrlState(Exec)
        goAbstract := true.B
      }
    }
  }.elsewhen(ctrlStateReg === CtrlState(Exec)) {

    // We can't just look at 'hartHalted' here, because
    // hartHaltedWrEn is overloaded to mean 'got an ebreak'
    // which may have happened when we were already halted.
    when(goReg === false.B && hartHaltedWrEn && (hartSelFuncs.hartIdToHartSel(hartHaltedId) === selectedHartReg)) {
      ctrlStateNxt := CtrlState(Waiting)
    }
    when(hartExceptionWrEn) {
      assert(
        hartExceptionId === 0.U,
        "Unexpected 'EXCEPTION' hart"
      ) // Chisel3 #540, %x, expected %x", hartExceptionId, 0.U)
      ctrlStateNxt := CtrlState(Waiting)
      errorException := true.B
    }
  }.elsewhen(ctrlStateReg === CtrlState(Custom)) {
    // assert(needCustom.B, "Should not be in custom state unless we need it.")
    goCustom := true.B
    // val (custom, customP) = customNode.in.head
    // when (custom.ready && custom.valid) {
    //   ctrlStateNxt := CtrlState(Waiting)
    // }
  }

  when(~io.i_dmactive || ~i_dmAuthenticated) {
    ctrlStateReg := CtrlState(Waiting)
  }.otherwise {
    ctrlStateReg := ctrlStateNxt
  }
  assert(
    (!io.i_dmactive || !hartExceptionWrEn || ctrlStateReg === CtrlState(Exec)),
    "Unexpected EXCEPTION write: should only get it in Debug Module EXEC state"
  )
}
