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

class TLDebugModuleOuterAsyncInterface(parameter: DMParameter) extends Bundle {
  val o_clock = Input(Clock())
  val o_reset = Input(if (parameter.useAsyncReset) AsyncReset() else Bool())
  // val intnode = Output(RegInit(VecInit(Seq.fill(parameter.nComponents) { false.B }))) // TODO
  val dmiClock = Input(Clock())
  val dmiReset = Input(Reset())

  /** Debug Module Interface bewteen DM and DTM
    *
    * The DTM provides access to one or more Debug Modules (DMs) using DMI
    */
  val dmi = Option.when(!parameter.apb)(Flipped(new DMIIO(parameter)))

  /** structure for top-level Debug Module signals which aren't the bus interfaces. */
  val ctrl = new DebugCtrlBundle(parameter.nComponents)

  /** conrol signals for Inner, generated in Outer */
  val o_innerCtrl = new DecoupledIO(new DebugInternalBundle(parameter.nComponents))

  /** debug interruption from Inner to Outer
    *
    * contains 2 type of debug interruption causes:
    *   - halt group
    *   - halt-on-reset
    */
  val o_hgDebugInt = Input(Vec(parameter.nComponents, Bool()))

  /** hart reset request to core */
  val hartResetReq = Option.when(parameter.hasHartResets)(Output(Vec(parameter.nComponents, Bool())))

  /** Authentication signal from core */
  val o_dmAuthenticated = Option.when(parameter.hasAuthentication)(Input(Bool()))
}

// wrap a Outer with a DMIToTL, derived by dmi clock & reset
class TLDebugModuleOuterAsync(val parameter: DMParameter)
    extends FixedIORawModule(new TLDebugModuleOuterAsyncInterface(parameter))
    with SerializableModule[DMParameter]
    with ImplicitClock
    with ImplicitReset {
  override protected def implicitClock: Clock = io.o_clock
  override protected def implicitReset: Reset = io.o_reset
  val cfg = parameter
  val supportHartArray = cfg.supportHartArray && (parameter.nComponents > 1) // no hart array if only one hart
  val nComponents = cfg.nComponents

  val dmiIO = Flipped(new DMIIO(parameter))
  val dmiInnerNode = dmiIO // TODO: TLAsyncCrossingSource
  val o_dmiNode =
    Flipped(axi4.bundle.verilog.irrevocable(parameter.axi4parameter)).asInstanceOf[AXI4RWIrrevocableVerilog]
  o_dmiNode := dmiIO

  withClockAndReset(io.dmiClock, io.dmiReset) {
    dmiIO <> io.dmi.get
  }

  val o_dmAuthenticated = io.o_dmAuthenticated
    .map(dma => dma // TODO: ResetSynchronizerShiftReg
    )
    .getOrElse(true.B)

  // ----DMCONTROL (The whole point of 'Outer' is to maintain this register on dmiClock (e.g. TCK) domain, so that it
  //               can be written even if 'Inner' is not being clocked or is in reset. This allows halting
  //               harts while the rest of the system is in reset. It doesn't really allow any other
  //               register accesses, which will keep returning 'busy' to the debugger interface.

  val DMCONTROLReset = WireInit(0.U.asTypeOf(new DMCONTROLFields()))
  val DMCONTROLNxt = WireInit(0.U.asTypeOf(new DMCONTROLFields()))
  val DMCONTROLReg = RegNext(next = DMCONTROLNxt, init = 0.U.asTypeOf(DMCONTROLNxt)).suggestName("DMCONTROLReg")

  val hartsel_mask = if (nComponents > 1) ((1 << parameter.MaxHartIdBits) - 1).U else 0.U
  val DMCONTROLWrData = WireInit(0.U.asTypeOf(new DMCONTROLFields()))

  val dmactiveWrEn = WireInit(false.B)
  val ndmresetWrEn = WireInit(false.B)
  val clrresethaltreqWrEn = WireInit(false.B)
  val setresethaltreqWrEn = WireInit(false.B)
  val hartselloWrEn = WireInit(false.B)
  val haselWrEn = WireInit(false.B)
  val ackhaveresetWrEn = WireInit(false.B)
  val hartresetWrEn = WireInit(false.B)
  val resumereqWrEn = WireInit(false.B)
  val haltreqWrEn = WireInit(false.B)

  val dmactive = DMCONTROLReg.dmactive

  DMCONTROLNxt := DMCONTROLReg
  when(~dmactive) {
    DMCONTROLNxt := DMCONTROLReset
  }.otherwise {
    when(o_dmAuthenticated && ndmresetWrEn) { DMCONTROLNxt.ndmreset := DMCONTROLWrData.ndmreset }
    when(o_dmAuthenticated && hartselloWrEn) { DMCONTROLNxt.hartsello := DMCONTROLWrData.hartsello & hartsel_mask }
    when(o_dmAuthenticated && haselWrEn) { DMCONTROLNxt.hasel := DMCONTROLWrData.hasel }
    when(o_dmAuthenticated && hartresetWrEn) { DMCONTROLNxt.hartreset := DMCONTROLWrData.hartreset }
    when(o_dmAuthenticated && haltreqWrEn) { DMCONTROLNxt.haltreq := DMCONTROLWrData.haltreq }
  }

  // Put this last to override its own effects.
  when(dmactiveWrEn) {
    DMCONTROLNxt.dmactive := DMCONTROLWrData.dmactive
  }

  // ----HARTINFO
  // DATA registers are mapped to memory. The dataaddr field of HARTINFO has only
  // 12 bits and assumes the DM base is 0.  If not at 0, then HARTINFO reads as 0
  // (implying nonexistence according to the Debug Spec).

  val HARTINFORdData = WireInit(0.U.asTypeOf(new HARTINFOFields()))
  if (cfg.atzero) when(o_dmAuthenticated) {
    HARTINFORdData.dataaccess := true.B
    HARTINFORdData.datasize := cfg.nAbstractDataWords.U
    HARTINFORdData.dataaddr := DsbRegAddrs.DATA.U
    HARTINFORdData.nscratch := cfg.nScratch.U
  }

  // --------------------------------------------------------------
  // Hart array mask and window
  //  hamask is hart array mask(1 bit per component), which doesn't include the hart selected by dmcontrol.hartsello
  //  HAWINDOWSEL selects a 32-bit slice of HAMASK to be visible for read/write in HAWINDOW
  // --------------------------------------------------------------

  val hamask = WireInit(VecInit(Seq.fill(nComponents) { false.B }))
  def haWindowSize = 32

  // The following need to be declared even if supportHartArray is false due to reference
  // at compile time by dmiNode.regmap
  val HAWINDOWSELWrData = WireInit(0.U.asTypeOf(new HAWINDOWSELFields()))
  val HAWINDOWSELWrEn = WireInit(false.B)

  val HAWINDOWRdData = WireInit(0.U.asTypeOf(new HAWINDOWFields()))
  val HAWINDOWWrData = WireInit(0.U.asTypeOf(new HAWINDOWFields()))
  val HAWINDOWWrEn = WireInit(false.B)

  /** whether the hart is selected */
  def hartSelected(hart: Int): Bool = {
    ((io.o_innerCtrl.bits.hartsel === hart.U) ||
    (if (supportHartArray) io.o_innerCtrl.bits.hasel && io.o_innerCtrl.bits.hamask(hart) else false.B))
  }

  val HAWINDOWSELNxt = WireInit(0.U.asTypeOf(new HAWINDOWSELFields()))
  val HAWINDOWSELReg = RegNext(next = HAWINDOWSELNxt, init = 0.U.asTypeOf(HAWINDOWSELNxt))

  if (supportHartArray) {
    val HAWINDOWSELReset = WireInit(0.U.asTypeOf(new HAWINDOWSELFields()))

    HAWINDOWSELNxt := HAWINDOWSELReg
    when(~dmactive || ~o_dmAuthenticated) {
      HAWINDOWSELNxt := HAWINDOWSELReset
    }.otherwise {
      when(HAWINDOWSELWrEn) {
        // Unneeded upper bits of HAWINDOWSEL are tied to 0.  Entire register is 0 if all harts fit in one window
        if (nComponents > haWindowSize) {
          HAWINDOWSELNxt.hawindowsel := HAWINDOWSELWrData.hawindowsel & ((1 << (log2Up(nComponents) - 5)) - 1).U
        } else {
          HAWINDOWSELNxt.hawindowsel := 0.U
        }
      }
    }
    val numHAMASKSlices = ((nComponents - 1) / haWindowSize) + 1
    HAWINDOWRdData.maskdata := 0.U // default, overridden below
    // for each slice,use a hamaskReg to store the selection info
    for (ii <- 0 until numHAMASKSlices) {
      val sliceMask =
        if (nComponents > ((ii * haWindowSize) + haWindowSize - 1))
          (BigInt(1) << haWindowSize) - 1 // All harts in this slice exist
        else (BigInt(1) << (nComponents - (ii * haWindowSize))) - 1 // Partial last slice
      val HAMASKRst = WireInit(0.U.asTypeOf(new HAWINDOWFields()))
      val HAMASKNxt = WireInit(0.U.asTypeOf(new HAWINDOWFields()))
      val HAMASKReg = RegNext(next = HAMASKNxt, init = 0.U.asTypeOf(HAMASKNxt))

      when(ii.U === HAWINDOWSELReg.hawindowsel) {
        HAWINDOWRdData.maskdata := HAMASKReg.asUInt & sliceMask.U
      }

      HAMASKNxt.maskdata := HAMASKReg.asUInt
      when(~dmactive || ~o_dmAuthenticated) {
        HAMASKNxt := HAMASKRst
      }.otherwise {
        when(HAWINDOWWrEn && (ii.U === HAWINDOWSELReg.hawindowsel)) {
          HAMASKNxt.maskdata := HAWINDOWWrData.maskdata
        }
      }

      // drive each slice of hamask with stored HAMASKReg or with new value being written
      for (jj <- 0 until haWindowSize) {
        if (((ii * haWindowSize) + jj) < nComponents) {
          val tempWrData = HAWINDOWWrData.maskdata.asBools
          val tempMaskReg = HAMASKReg.asUInt.asBools
          when(HAWINDOWWrEn && (ii.U === HAWINDOWSELReg.hawindowsel)) {
            hamask(ii * haWindowSize + jj) := tempWrData(jj)
          }.otherwise {
            hamask(ii * haWindowSize + jj) := tempMaskReg(jj)
          }
        }
      }
    }
  }

  // --------------------------------------------------------------
  // Halt-on-reset
  //  hrmaskReg is current set of harts that should halt-on-reset
  //    Reset state (dmactive=0) is all zeroes
  //    Bits are set by writing 1 to DMCONTROL.setresethaltreq
  //    Bits are cleared by writing 1 to DMCONTROL.clrresethaltreq
  //    Spec says if both are 1, then clrresethaltreq is executed
  //  hrmask is the halt-on-reset mask which will be sent to inner
  // --------------------------------------------------------------

  val hrmask = Wire(Vec(nComponents, Bool()))
  val hrmaskNxt = Wire(Vec(nComponents, Bool()))
  val hrmaskReg = RegNext(next = hrmaskNxt, init = 0.U.asTypeOf(hrmaskNxt)).suggestName("hrmaskReg")

  hrmaskNxt := hrmaskReg
  for (component <- 0 until nComponents) {
    when(~dmactive || ~o_dmAuthenticated) {
      hrmaskNxt(component) := false.B
    }.elsewhen(clrresethaltreqWrEn && DMCONTROLWrData.clrresethaltreq && hartSelected(component)) {
      hrmaskNxt(component) := false.B
    }.elsewhen(setresethaltreqWrEn && DMCONTROLWrData.setresethaltreq && hartSelected(component)) {
      hrmaskNxt(component) := true.B
    }
  }
  hrmask := hrmaskNxt

  val dmControlRegFields = RegFieldGroup(
    "dmcontrol",
    Some("debug module control register"),
    Seq(
      WNotifyVal(
        1,
        DMCONTROLReg.dmactive & io.ctrl.dmactiveAck,
        DMCONTROLWrData.dmactive,
        dmactiveWrEn,
        RegFieldDesc("dmactive", "debug module active", reset = Some(0))
      ),
      WNotifyVal(
        1,
        DMCONTROLReg.ndmreset,
        DMCONTROLWrData.ndmreset,
        ndmresetWrEn,
        RegFieldDesc("ndmreset", "debug module reset output", reset = Some(0))
      ),
      WNotifyVal(
        1,
        0.U,
        DMCONTROLWrData.clrresethaltreq,
        clrresethaltreqWrEn,
        RegFieldDesc("clrresethaltreq", "clear reset halt request", reset = Some(0), access = RegFieldAccessType.W)
      ),
      WNotifyVal(
        1,
        0.U,
        DMCONTROLWrData.setresethaltreq,
        setresethaltreqWrEn,
        RegFieldDesc("setresethaltreq", "set reset halt request", reset = Some(0), access = RegFieldAccessType.W)
      ),
      RegField(12),
      if (nComponents > 1)
        WNotifyVal(
          parameter.MaxHartIdBits,
          DMCONTROLReg.hartsello,
          DMCONTROLWrData.hartsello,
          hartselloWrEn,
          RegFieldDesc("hartsello", "hart select low", reset = Some(0))
        )
      else RegField(1),
      if (nComponents > 1) RegField(10 - parameter.MaxHartIdBits)
      else RegField(9),
      if (supportHartArray)
        WNotifyVal(
          1,
          DMCONTROLReg.hasel,
          DMCONTROLWrData.hasel,
          haselWrEn,
          RegFieldDesc("hasel", "hart array select", reset = Some(0))
        )
      else RegField(1),
      RegField(1),
      WNotifyVal(
        1,
        0.U,
        DMCONTROLWrData.ackhavereset,
        ackhaveresetWrEn,
        RegFieldDesc("ackhavereset", "acknowledge reset", reset = Some(0), access = RegFieldAccessType.W)
      ),
      if (cfg.hasHartResets)
        WNotifyVal(
          1,
          DMCONTROLReg.hartreset,
          DMCONTROLWrData.hartreset,
          hartresetWrEn,
          RegFieldDesc("hartreset", "hart reset request", reset = Some(0))
        )
      else RegField(1),
      WNotifyVal(
        1,
        0.U,
        DMCONTROLWrData.resumereq,
        resumereqWrEn,
        RegFieldDesc("resumereq", "resume request", reset = Some(0), access = RegFieldAccessType.W)
      ),
      WNotifyVal(
        1,
        DMCONTROLReg.haltreq,
        DMCONTROLWrData.haltreq,
        haltreqWrEn, // Spec says W, but maintaining previous behavior
        RegFieldDesc("haltreq", "halt request", reset = Some(0))
      )
    )
  )

  val hartinfoRegFields = RegFieldGroup(
    "dmi_hartinfo",
    Some("hart information"),
    Seq(
      RegField.r(
        12,
        HARTINFORdData.dataaddr,
        RegFieldDesc("dataaddr", "data address", reset = Some(if (cfg.atzero) DsbRegAddrs.DATA else 0))
      ),
      RegField.r(
        4,
        HARTINFORdData.datasize,
        RegFieldDesc(
          "datasize",
          "number of DATA registers",
          reset = Some(if (cfg.atzero) cfg.nAbstractDataWords else 0)
        )
      ),
      RegField.r(
        1,
        HARTINFORdData.dataaccess,
        RegFieldDesc("dataaccess", "data access type", reset = Some(if (cfg.atzero) 1 else 0))
      ),
      RegField(3),
      RegField.r(
        4,
        HARTINFORdData.nscratch,
        RegFieldDesc("nscratch", "number of scratch registers", reset = Some(if (cfg.atzero) cfg.nScratch else 0))
      )
    )
  )

  // --------------------------------------------------------------
  // DMI register decoder for Outer
  // --------------------------------------------------------------

  // regmap addresses are byte offsets from lowest address
  def DMI_DMCONTROL_OFFSET = 0
  def DMI_HARTINFO_OFFSET = ((DMI_HARTINFO - DMI_DMCONTROL) << 2)
  def DMI_HAWINDOWSEL_OFFSET = ((DMI_HAWINDOWSEL - DMI_DMCONTROL) << 2)
  def DMI_HAWINDOW_OFFSET = ((DMI_HAWINDOW - DMI_DMCONTROL) << 2)

  // val omRegMap = o_dmiNode.regmap(
  regmap(
    o_dmiNode.viewAs[AXI4RWIrrevocable],
    0,
    false,
    DMI_DMCONTROL_OFFSET -> dmControlRegFields,
    DMI_HARTINFO_OFFSET -> hartinfoRegFields,
    DMI_HAWINDOWSEL_OFFSET -> (if (supportHartArray && (nComponents > 32))
                                 Seq(
                                   WNotifyVal(
                                     log2Up(nComponents) - 5,
                                     HAWINDOWSELReg.hawindowsel,
                                     HAWINDOWSELWrData.hawindowsel,
                                     HAWINDOWSELWrEn,
                                     RegFieldDesc("hawindowsel", "hart array window select", reset = Some(0))
                                   )
                                 )
                               else Nil),
    DMI_HAWINDOW_OFFSET -> (if (supportHartArray)
                              Seq(
                                WNotifyVal(
                                  if (nComponents > 31) 32 else nComponents,
                                  HAWINDOWRdData.maskdata,
                                  HAWINDOWWrData.maskdata,
                                  HAWINDOWWrEn,
                                  RegFieldDesc(
                                    "hawindow",
                                    "hart array window",
                                    reset = Some(0),
                                    volatile = (nComponents > 32)
                                  )
                                )
                              )
                            else Nil)
  )

  // --------------------------------------------------------------
  // Interrupt Registers
  // --------------------------------------------------------------

  val debugIntNxt = WireInit(VecInit(Seq.fill(nComponents) { false.B }))
  val debugIntRegs = RegNext(next = debugIntNxt, init = 0.U.asTypeOf(debugIntNxt)).suggestName("debugIntRegs")

  debugIntNxt := debugIntRegs
  // for (component <- 0 until nComponents) {
  //   io.intnode(component)(0) := debugIntRegs(component) | io.o_hgDebugInt(component)
  // } // TODO

  // sends debug interruption to Core when dmcs.haltreq is set,
  for (component <- 0 until nComponents) {
    when(~dmactive || ~o_dmAuthenticated) {
      debugIntNxt(component) := false.B
    }.otherwise {
      when(
        haltreqWrEn && ((DMCONTROLWrData.hartsello === component.U)
          || (if (supportHartArray) DMCONTROLWrData.hasel && hamask(component) else false.B))
      ) {
        debugIntNxt(component) := DMCONTROLWrData.haltreq
      }
    }
  }
  // Halt request registers are set & cleared by writes to DMCONTROL.haltreq
  // resumereq also causes the core to execute a 'dret',
  // so resumereq is passed through to Inner.
  // hartsel/hasel/hamask must also be used by the DebugModule state machine,
  // so it is passed to Inner.

  // These registers ensure that requests to dmInner are not lost if inner clock isn't running or requests occur too close together.
  // If the innerCtrl async queue is not ready, the notification will be posted and held until ready is received.
  // Additional notifications that occur while one is already waiting update the pending data so that the last value written is sent.
  // Volatile events resumereq and ackhavereset are registered when they occur and remain pending until ready is received.
  val innerCtrlValid = Wire(Bool())
  val innerCtrlValidReg = RegInit(false.B).suggestName("innerCtrlValidReg")
  val innerCtrlResumeReqReg = RegInit(false.B).suggestName("innerCtrlResumeReqReg")
  val innerCtrlAckHaveResetReg = RegInit(false.B).suggestName("innerCtrlAckHaveResetReg")

  innerCtrlValid := hartselloWrEn | resumereqWrEn | ackhaveresetWrEn | setresethaltreqWrEn | clrresethaltreqWrEn | haselWrEn |
    (HAWINDOWWrEn & supportHartArray.B)

  innerCtrlValidReg := io.o_innerCtrl.valid & ~io.o_innerCtrl.ready // Hold innerctrl request until the async queue accepts it
  innerCtrlResumeReqReg := io.o_innerCtrl.bits.resumereq & ~io.o_innerCtrl.ready // Hold resumereq until accepted
  innerCtrlAckHaveResetReg := io.o_innerCtrl.bits.ackhavereset & ~io.o_innerCtrl.ready // Hold ackhavereset until accepted

  io.o_innerCtrl.valid := innerCtrlValid | innerCtrlValidReg
  io.o_innerCtrl.bits.hartsel := Mux(hartselloWrEn, DMCONTROLWrData.hartsello, DMCONTROLReg.hartsello)
  io.o_innerCtrl.bits.resumereq := (resumereqWrEn & DMCONTROLWrData.resumereq) | innerCtrlResumeReqReg
  io.o_innerCtrl.bits.ackhavereset := (ackhaveresetWrEn & DMCONTROLWrData.ackhavereset) | innerCtrlAckHaveResetReg
  io.o_innerCtrl.bits.hrmask := hrmask
  if (supportHartArray) {
    io.o_innerCtrl.bits.hasel := Mux(haselWrEn, DMCONTROLWrData.hasel, DMCONTROLReg.hasel)
    io.o_innerCtrl.bits.hamask := hamask
  } else {
    io.o_innerCtrl.bits.hasel := DontCare
    io.o_innerCtrl.bits.hamask := DontCare
  }

  io.ctrl.ndreset := DMCONTROLReg.ndmreset
  io.ctrl.dmactive := DMCONTROLReg.dmactive

  // hart reset mechanism implementation
  if (cfg.hasHartResets) {
    val hartResetNxt = Wire(Vec(nComponents, Bool()))
    val hartResetReg = RegNext(next = hartResetNxt, init = 0.U.asTypeOf(hartResetNxt))

    for (component <- 0 until nComponents) {
      hartResetNxt(component) := DMCONTROLReg.hartreset & hartSelected(component)
      io.hartResetReq.get(component) := hartResetReg(component)
    }
  }

}
