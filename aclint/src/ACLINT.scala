// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2024 Jiuyang Liu <liu@jiuyang.me>

package org.chipsalliance.uncore.aclint

import chisel3._
import chisel3.experimental.dataview.DataViewable
import chisel3.experimental.hierarchy.{instantiable, Instance, Instantiate}
import chisel3.experimental.{SerializableModule, SerializableModuleParameter}
import chisel3.probe.{define, Probe, ProbeValue}
import chisel3.properties.{AnyClassType, Class, Property}
import org.chipsalliance.amba.RegMapper.regmap
import org.chipsalliance.amba._
import org.chipsalliance.amba.axi4.bundle._

object ACLINTParameter {
  implicit def rwP: upickle.default.ReadWriter[ACLINTParameter] =
    upickle.default.macroRW
}

/** Parameter of [[ACLINT]] */
case class ACLINTParameter(
                            useAsyncReset:  Boolean,
                            mtimeBase:      BigInt,
                            mtimecmpBase:   BigInt,
                            harts:          Int,
                            satelliteMtime: Boolean,
                            axi4parameter:  AXI4BundleParameter)
  extends SerializableModuleParameter {
  require(!satelliteMtime, "unimpl")
  require(axi4parameter.isRW)
}

/** Verification IO of [[ACLINT]] */
class ACLINTProbe(parameter: ACLINTParameter) extends Bundle {}

/** Metadata of [[ACLINT]]. */
@instantiable
class ACLINTOM(parameter: ACLINTParameter) extends Class {
  val useAsyncReset: Property[Boolean] = IO(Output(Property[Boolean]()))
  useAsyncReset := Property(parameter.useAsyncReset)
}

/** Interface of [[ACLINT]]. */
class ACLINTInterface(parameter: ACLINTParameter) extends Bundle {
  val clock = Input(Clock())
  val reset = Input(if (parameter.useAsyncReset) AsyncReset() else Bool())
  val probe = Output(Probe(new ACLINTProbe(parameter), layers.Verification))
  val om = Output(Property[AnyClassType]())
  // TODO: do we really need two axi ports for two register groups? I'm sure.
  val mtimerTime =
    Flipped(axi4.bundle.verilog.irrevocable(parameter.axi4parameter)).asInstanceOf[AXI4RWIrrevocableVerilog]
  val mtimerCompare =
    Flipped(axi4.bundle.verilog.irrevocable(parameter.axi4parameter)).asInstanceOf[AXI4RWIrrevocableVerilog]
  val mip = Output(Vec(parameter.harts, Bool()))
}

/** Hardware Implementation of ACLINT */
@instantiable
class ACLINT(val parameter: ACLINTParameter)
  extends FixedIORawModule(new ACLINTInterface(parameter))
    with SerializableModule[ACLINTParameter]
    with ImplicitClock
    with ImplicitReset {
  override protected def implicitClock: Clock = io.clock

  override protected def implicitReset: Reset = io.reset

  // Assign Probe
  val probeWire: ACLINTProbe = Wire(new ACLINTProbe(parameter))
  define(io.probe, ProbeValue(probeWire))

  // Assign Metadata
  val omInstance: Instance[ACLINTOM] = Instantiate(new ACLINTOM(parameter))
  io.om := omInstance.getPropertyReference.asAnyClassType
  // 2. Machine-level Timer Device (MTIMER)
  /** MTIME Register (Offset: 0x00000000) The MTIME register is a 64-bit read-write register that contains the number of
   * cycles counted based on a fixed reference frequency. On MTIMER device reset, the MTIME register is cleared to
   * zero.
   */
  val mtime = RegInit(0.U(64.W))

  regmap(
    io.mtimerTime.viewAs[AXI4RWIrrevocable],
    0,
    false,
    0 -> RegField.bytes(mtime, Some(RegFieldDesc("MTIME", "Machine-level time counter")))
  )

  /** MTIMECMP Registers (Offsets: 0x00000000 - 0x00007FF0) The MTIMECMP registers are per-HART 64-bit read-write
   * registers. It contains the MTIME register value at which machine-level timer interrupt is to be triggered for the
   * corresponding HART. The machine-level timer interrupt of a HART is pending whenever MTIME is greater than or equal
   * to the value in the corresponding MTIMECMP register whereas the machine-level timer interrupt of a HART is cleared
   * whenever MTIME is less than the value of the corresponding MTIMECMP register. The machine-level timer interrupt is
   * reflected in the MTIP bit of the mip CSR. On MTIMER device reset, the MTIMECMP registers are in unknown state.
   */
  val mtimecmp = Seq.tabulate(parameter.harts)(idx => Reg(UInt(64.W)).suggestName(s"mtimecmp_${idx}"))

  regmap(
    io.mtimerCompare.viewAs[AXI4RWIrrevocable],
    0,
    false,
    mtimecmp.zipWithIndex.map { case (reg, idx) =>
      idx -> RegField.bytes(
        reg,
        Some(RegFieldDesc(s"MTIMECMP$idx", s"HART index $idx machine-level time compare", reset = None))
      )
    }: _*
  )

  io.mip := VecInit(mtimecmp.map(_ >= mtime))

  // 3. Machine-level Software Interrupt Device (MSWI)

  // 4. Supervisor-level Software Interrupt Device (SSWI)
}
