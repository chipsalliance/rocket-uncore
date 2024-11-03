// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2024 Jiuyang Liu <liu@jiuyang.me>

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

object Instructions {
  def JAL = BitPat("b?????????????????????????1101111")
  def LW = BitPat("b?????????????????010?????0000011")
  def SW = BitPat("b?????????????????010?????0100011")
  def CSRRW = BitPat("b?????????????????001?????1110011")
  def ADDI = BitPat("b?????????????????000?????0010011")
  def ADDIW = BitPat("b?????????????????000?????0011011")
  def EBREAK = BitPat("b00000000000100000000000001110011")
}

object CSRs {
  val dscratch1 = 0x7b3
}

object DsbBusConsts {
  def sbAddrWidth = 12
  def sbIdWidth = 10
}

object DsbRegAddrs {

  // These are used by the ROM.
  def HALTED = 0x100
  def GOING = 0x104
  def RESUMING = 0x108
  def EXCEPTION = 0x10c

  def WHERETO = 0x300
  // This needs to be aligned for up to lq/sq

  // This shows up in HartInfo, and needs to be aligned
  // to enable up to LQ/SQ instructions.
  def DATA = 0x380

  // We want DATA to immediately follow PROGBUF so that we can
  // use them interchangeably. Leave another slot if there is an
  // implicit ebreak.
  def PROGBUF(cfg: DMParameter) = {
    val tmp = DATA - (cfg.nProgramBufferWords * 4)
    if (cfg.hasImplicitEbreak) (tmp - 4) else tmp
  }
  // This is unused if hasImpEbreak is false, and just points to the end of the PROGBUF.
  def IMPEBREAK(cfg: DMParameter) = { DATA - 4 }

  // We want abstract to be immediately before PROGBUF
  // because we auto-generate 2 (or 5) instructions.
  def ABSTRACT(cfg: DMParameter) = PROGBUF(cfg) - (cfg.nAbstractInstructions * 4)

  def FLAGS = 0x400
  def ROMBASE = 0x800

}

/** Enumerations used both in the hardware and in the configuration specification.
  */

object DebugModuleAccessType extends scala.Enumeration {
  type DebugModuleAccessType = Value
  val Access8Bit, Access16Bit, Access32Bit, Access64Bit, Access128Bit = Value
}

object DebugAbstractCommandError extends scala.Enumeration {
  type DebugAbstractCommandError = Value
  val Success, ErrBusy, ErrNotSupported, ErrException, ErrHaltResume = Value
}

object DebugAbstractCommandType extends scala.Enumeration {
  type DebugAbstractCommandType = Value
  val AccessRegister, QuickAccess = Value
}

object DMParameter {
  implicit def rwP: upickle.default.ReadWriter[DMParameter] =
    upickle.default.macroRW
}

/** Parameters exposed to the top-level design, set based on external requirements, etc.
  *
  * This object checks that the parameters conform to the full specification. The implementation which receives this
  * object can perform more checks on what that implementation actually supports.
  * @param nComponents
  *   Number of components to support debugging.
  * @param baseAddress
  *   Base offest for debugEntry and debugException
  * @param nDMIAddrSize
  *   Size of the Debug Bus Address
  * @param nAbstractDataWords
  *   Number of 32-bit words for Abstract Commands
  * @param nProgramBufferWords
  *   Number of 32-bit words for Program Buffer
  * @param hasBusMaster
  *   Whether or not a bus master should be included
  * @param clockGate
  *   Whether or not to use dmactive as the clockgate for debug module
  * @param maxSupportedSBAccess
  *   Maximum transaction size supported by System Bus Access logic.
  * @param supportQuickAccess
  *   Whether or not to support the quick access command.
  * @param supportHartArray
  *   Whether or not to implement the hart array register (if >1 hart).
  * @param nHaltGroups
  *   Number of halt groups
  * @param nExtTriggers
  *   Number of external triggers
  * @param hasHartResets
  *   Feature to reset all the currently selected harts
  * @param hasImplicitEbreak
  *   There is an additional RO program buffer word containing an ebreak
  * @param crossingHasSafeReset
  *   Include "safe" logic in Async Crossings so that only one side needs to be reset.
  */
case class DMParameter(
  useAsyncReset:        Boolean,
  nComponents:          Int = 2,
  baseAddress:          BigInt = BigInt(0),
  nDMIAddrSize:         Int = 7,
  nAbstractDataWords:   Int = 4,
  nProgramBufferWords:  Int = 16,
  hasBusMaster:         Boolean = false,
  clockGate:            Boolean = true,
  maxSupportedSBAccess: Int = 32,
  supportQuickAccess:   Boolean = false,
  supportHartArray:     Boolean = true,
  nHaltGroups:          Int = 1,
  nExtTriggers:         Int = 0,
  hasHartResets:        Boolean = false,
  hasImplicitEbreak:    Boolean = false,
  crossingHasSafeReset: Boolean = true,
  nScratch:             Int = 1,
  hasAuthentication:    Boolean = false,
  MaxHartIdBits:        Int = 2,
  dmi:                  Boolean = true,
  jtag:                 Boolean = true,
  cjtag:                Boolean = false,
  apb:                  Boolean = false,
  axi4parameter:        AXI4BundleParameter)
    extends SerializableModuleParameter {

  require((nDMIAddrSize >= 7) && (nDMIAddrSize <= 32), s"Legal DMIAddrSize is 7-32, not ${nDMIAddrSize}")

  require(
    (nAbstractDataWords > 0) && (nAbstractDataWords <= 16),
    s"Legal nAbstractDataWords is 0-16, not ${nAbstractDataWords}"
  )
  require(
    (nProgramBufferWords >= 0) && (nProgramBufferWords <= 16),
    s"Legal nProgramBufferWords is 0-16, not ${nProgramBufferWords}"
  )

  require(nHaltGroups < 32, s"Legal nHaltGroups is 0-31, not ${nHaltGroups}")
  require(nExtTriggers <= 16, s"Legal nExtTriggers is 0-16, not ${nExtTriggers}")

  if (supportQuickAccess) {
    // TODO: Check that quick access requirements are met.
  }

  // def address = AddressSet(baseAddress, 0xFFF) // TODO
  /** the base address of DM */
  def atzero = (baseAddress == 0)

  /** The number of generated instructions
    *
    * When the base address is not zero, we need more instruction also, more dscratch registers) to load/store memory
    * mapped data register because they may no longer be directly addressible with x0 + 12-bit imm
    */
  def nAbstractInstructions = if (atzero) 2 else 5
  def debugEntry:     BigInt = baseAddress + 0x800
  def debugException: BigInt = baseAddress + 0x808
  def nDscratch:      Int = if (atzero) 1 else 2
}

/** Functional parameters exposed to the design configuration.
  *
  * hartIdToHartSel: For systems where hart ids are not 1:1 with hartsel, provide the mapping. hartSelToHartId: Provide
  * inverse mapping of the above
  */
case class DebugModuleHartSelFuncs(
  hartIdToHartSel: (UInt) => UInt = (x: UInt) => x,
  hartSelToHartId: (UInt) => UInt = (x: UInt) => x)

class DebugExtTriggerOut(val nExtTriggers: Int) extends Bundle {
  val req = Output(UInt(nExtTriggers.W))
  val ack = Input(UInt(nExtTriggers.W))
}

class DebugExtTriggerIn(val nExtTriggers: Int) extends Bundle {
  val req = Input(UInt(nExtTriggers.W))
  val ack = Output(UInt(nExtTriggers.W))
}

class DebugExtTriggerIO(val nExtTriggers: Int) extends Bundle {
  val out = new DebugExtTriggerOut(nExtTriggers)
  val in = new DebugExtTriggerIn(nExtTriggers)
}

class DebugAuthenticationIO() extends Bundle {
  val dmactive = Output(Bool())
  val dmAuthWrite = Output(Bool())
  val dmAuthRead = Output(Bool())
  val dmAuthWdata = Output(UInt(32.W))
  val dmAuthBusy = Input(Bool())
  val dmAuthRdata = Input(UInt(32.W))
  val dmAuthenticated = Input(Bool())
}

// *****************************************
// Module Interfaces
//
// *****************************************

/** Control signals for Inner, generated in Outer
  * {{{
  * run control: resumreq, ackhavereset, halt-on-reset mask
  * hart select: hasel, hartsel and the hart array mask
  * }}}
  */
class DebugInternalBundle(val nComponents: Int) extends Bundle {

  /** resume request */
  val resumereq = Bool()

  /** hart select */
  val hartsel = UInt(10.W)

  /** reset acknowledge */
  val ackhavereset = Bool()

  /** hart array enable */
  val hasel = Bool()

  /** hart array mask */
  val hamask = Vec(nComponents, Bool())

  /** halt-on-reset mask */
  val hrmask = Vec(nComponents, Bool())
}

/** structure for top-level Debug Module signals which aren't the bus interfaces. */
class DebugCtrlBundle(nComponents: Int) extends Bundle {

  /** debug availability status for all harts */
  val debugUnavail = Input(Vec(nComponents, Bool()))

  /** reset signal
    *
    * for every part of the hardware platform, including every hart, except for the DM and any logic required to access
    * the DM
    */
  val ndreset = Output(Bool())

  /** reset signal for the DM itself */
  val dmactive = Output(Bool())

  /** dmactive acknowlege */
  val dmactiveAck = Input(Bool())
}

// *****************************************
// Debug Module
//
// *****************************************

/** Parameterized version of the Debug Module defined in the RISC-V Debug Specification
  *
  * DebugModule is a slave to two asynchronous masters: The Debug Bus (DMI) -- This is driven by an external debugger
  *
  * The System Bus -- This services requests from the cores. Generally this interface should only be active at the
  * request of the debugger, but the Debug Module may also provide the default MTVEC since it is mapped to address 0x0.
  *
  * DebugModule is responsible for control registers and RAM, and Debug ROM. It runs partially off of the dmiClk (e.g.
  * TCK) and the TL clock. Therefore, it is divided into "Outer" portion (running off dmiClock and dmiReset) and "Inner"
  * (running off tl_clock and tl_reset). This allows DMCONTROL.haltreq, hartsel, hasel, hawindowsel, hawindow, dmactive,
  * and ndreset to be modified even while the Core is in reset or not being clocked. Not all reads from the Debugger to
  * the Debug Module will actually complete in these scenarios either, they will just block until tl_clock and tl_reset
  * allow them to complete. This is not strictly necessary for proper debugger functionality.
  */

// Local reg mapper function : Notify when written, but give the value as well.
object WNotifyWire {
  def apply(n: Int, value: UInt, set: Bool, name: String, desc: String): RegField = {
    RegField(
      n,
      0.U,
      RegWriteFn((valid, data) => {
        set := valid
        value := data
        true.B
      }),
      Some(RegFieldDesc(name = name, desc = desc, access = RegFieldAccessType.W))
    )
  }
}

// Local reg mapper function : Notify when accessed either as read or write.
object RWNotify {
  def apply(n: Int, rVal: UInt, wVal: UInt, rNotify: Bool, wNotify: Bool, desc: Option[RegFieldDesc] = None)
    : RegField = {
    RegField(
      n,
      RegReadFn((ready) => { rNotify := ready; (true.B, rVal) }),
      RegWriteFn((valid, data) => {
        wNotify := valid
        when(valid) { wVal := data }
        true.B
      }),
      desc
    )
  }
}

// Local reg mapper function : Notify with value when written, take read input as presented.
// This allows checking or correcting the write value before storing it in the register field.
object WNotifyVal {
  def apply(n: Int, rVal: UInt, wVal: UInt, wNotify: Bool, desc: RegFieldDesc): RegField = {
    RegField(
      n,
      rVal,
      RegWriteFn((valid, data) => {
        wNotify := valid
        wVal := data
        true.B
      }),
      desc
    )
  }
}

/** Verification IO of [[DM]] */
class DMProbe(parameter: DMParameter) extends Bundle {}

/** Metadata of [[DM]]. */
@instantiable
class DMOM(parameter: DMParameter) extends Class {
  val useAsyncReset: Property[Boolean] = IO(Output(Property[Boolean]()))
  useAsyncReset := Property(parameter.useAsyncReset)
}

/** Interface of [[DM]]. */
class DMInterface(parameter: DMParameter) extends Bundle {
  val probe = Output(Probe(new DMProbe(parameter), layers.Verification))
  val om = Output(Property[AnyClassType]())

  val clock = Input(Clock())
  val reset = Input(if (parameter.useAsyncReset) AsyncReset() else Bool())

  val debug_clock = Input(Clock())
  val debug_reset = Input(Reset())
  val tl_clock = Input(Clock())
  val tl_reset = Input(Reset())

  /** Debug control signals generated in Outer */
  val ctrl = new DebugCtrlBundle(parameter.nComponents)

  /** Debug Module Interface bewteen DM and DTM
    *
    * The DTM provides access to one or more Debug Modules (DMs) using DMI
    */
  val dmi = Flipped(new ClockedDMIIO(parameter))
  // TODO
  // val extTrigger = Option.when(parameter.nExtTriggers > 0)(new DebugExtTriggerIO(parameter.nExtTriggers))

  /** vector to indicate which hart is in reset
    *
    * dm receives it from core and sends it to Inner
    */
  val hartIsInReset = Input(Vec(parameter.nComponents, Bool()))

  /** hart reset request generated by hartreset-logic in Outer */
  val hartResetReq = Option.when(parameter.hasHartResets)(Output(Vec(parameter.nComponents, Bool())))

  /** Debug Authentication signals from core */
  // TODO
  // val auth = Option.when(parameter.hasAuthentication)(new DebugAuthenticationIO())
  val node = Input(
    Flipped(axi4.bundle.verilog.irrevocable(parameter.axi4parameter)).asInstanceOf[AXI4RWIrrevocableVerilog]
  )
  val intnode = Output(WireInit(VecInit(Seq.fill(parameter.nComponents) { false.B })))
}

/** Create a version of the TLDebugModule which includes a synchronization interface internally for the DMI. This is no
  * longer optional outside of this module because the Clock must run when tl_clock isn't running or tl_reset is
  * asserted.
  */

@instantiable
class DM(val parameter: DMParameter)
    extends FixedIORawModule(new DMInterface(parameter))
    with SerializableModule[DMParameter]
    with ImplicitClock
    with ImplicitReset {
  override protected def implicitClock: Clock = io.clock
  override protected def implicitReset: Reset = io.reset

  // Assign Probe
  val probeWire: DMProbe = Wire(new DMProbe(parameter))
  define(io.probe, ProbeValue(probeWire))

  // Assign Metadata
  val omInstance: Instance[DMOM] = Instantiate(new DMOM(parameter))
  io.om := omInstance.getPropertyReference.asAnyClassType

  val dmOuter: TLDebugModuleOuterAsync = Module(new TLDebugModuleOuterAsync(parameter))
  val dmInner: TLDebugModuleInnerAsync = Module(new TLDebugModuleInnerAsync(parameter))

  dmInner.io.i_tlNode := io.node
  dmInner.io.i_dmiNode := dmOuter.dmiInnerNode

  val nComponents = parameter.nComponents

  dmOuter.io.dmi.foreach { dmOuterDMI =>
    dmOuterDMI <> io.dmi.dmi
  }
  dmOuter.io.dmiReset := io.dmi.dmiReset
  dmOuter.io.dmiClock := io.dmi.dmiClock

  dmInner.io.i_rf_reset := io.debug_reset
  dmInner.io.i_debugUnavail := io.ctrl.debugUnavail

  dmInner.io.i_innerCtrl <> dmOuter.io.o_innerCtrl
  dmInner.io.i_dmactive := dmOuter.io.ctrl.dmactive
  dmOuter.io.o_hgDebugInt := dmInner.io.i_hgDebugInt

  io.ctrl <> dmOuter.io.ctrl
  io.hartResetReq.foreach { x => dmOuter.io.hartResetReq.foreach { y => x := y } }
  // io.auth.foreach { x => dmOuter.io.o_dmAuthenticated.get := x.dmAuthenticated }

  // io.extTrigger.foreach { x => dmInner.io.extTrigger.foreach { y => x <> y } }
  dmInner.io.hartIsInReset := io.hartIsInReset
  // io.auth.foreach { x => dmInner.io.i_auth.foreach { y => x <> y } }
}
