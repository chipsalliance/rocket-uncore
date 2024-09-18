// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2016-2017 SiFive, Inc
// SPDX-FileCopyrightText: 2024 Jiuyang Liu <liu@jiuyang.me>

package org.chipsalliance.uncore.plic

import chisel3._
import chisel3.experimental.hierarchy.{instantiable, Instance, Instantiate}
import chisel3.experimental.{SerializableModule, SerializableModuleParameter}
import chisel3.probe.{define, Probe, ProbeValue}
import chisel3.properties.{AnyClassType, Class, Property}
import chisel3.util.log2Ceil
import org.chipsalliance.amba._
import org.chipsalliance.amba.axi4.bundle._

object PLICParameter {
  implicit def rwP: upickle.default.ReadWriter[PLICParameter] =
    upickle.default.macroRW
}

/** Parameter of [[PLIC]] */
case class PLICParameter(
  useAsyncReset: Boolean,
  base:          BigInt,
  harts:         Int,
  sources:       Int,
  maxPriorities: Int,
  axi4parameter: AXI4BundleParameter)
    extends SerializableModuleParameter {
  def nDevices: Int = sources

  def minPriorities = math.min(maxPriorities, nDevices)

  def nPriorities = (1 << log2Ceil(minPriorities + 1)) - 1 // round up to next 2^n-1

  val prioBits = log2Ceil(nPriorities + 1)

  def nHarts = harts
}

/** Verification IO of [[PLIC]] */
class PLICProbe(parameter: PLICParameter) extends Bundle {}

/** Metadata of [[PLIC]]. */
@instantiable
class PLICOM(parameter: PLICParameter) extends Class {
  val useAsyncReset: Property[Boolean] = IO(Output(Property[Boolean]()))
  useAsyncReset := Property(parameter.useAsyncReset)
}

/** Interface of [[PLIC]]. */
class PLICInterface(parameter: PLICParameter) extends Bundle {
  val clock = Input(Clock())
  val reset = Input(if (parameter.useAsyncReset) AsyncReset() else Bool())
  val probe = Output(Probe(new PLICProbe(parameter), layers.Verification))
  val om = Output(Property[AnyClassType]())
  val plic = Flipped(axi4.bundle.verilog.irrevocable(parameter.axi4parameter)).asInstanceOf[AXI4RWIrrevocableVerilog]
  val source = Input(Vec(parameter.sources, Bool()))
  val mip = Output(Vec(parameter.harts, Bool()))
  val sip = Output(Vec(parameter.harts, Bool()))
}

/** Hardware Implementation of PLIC */
@instantiable
class PLIC(val parameter: PLICParameter)
    extends FixedIORawModule(new PLICInterface(parameter))
    with SerializableModule[PLICParameter]
    with ImplicitClock
    with ImplicitReset {
  override protected def implicitClock: Clock = io.clock

  override protected def implicitReset: Reset = io.reset

  // Assign Probe
  val probeWire: PLICProbe = Wire(new PLICProbe(parameter))
  define(io.probe, ProbeValue(probeWire))

  // Assign Metadata
  val omInstance: Instance[PLICOM] = Instantiate(new PLICOM(parameter))
  io.om := omInstance.getPropertyReference.asAnyClassType

  def priorityRegDesc(i: Int) =
    RegFieldDesc(
      name = s"priority_$i",
      desc = s"Acting priority of interrupt source $i",
      group = Some(s"priority_${i}"),
      groupDesc = Some(s"Acting priority of interrupt source $i"),
      reset = if (parameter.nPriorities > 0) None else Some(1)
    )

  def pendingRegDesc(i: Int) =
    RegFieldDesc(
      name = s"pending_$i",
      desc = s"Set to 1 if interrupt source $i is pending, regardless of its enable or priority setting.",
      group = Some("pending"),
      groupDesc = Some("Pending Bit Array. 1 Bit for each interrupt source."),
      volatile = true
    )

  def enableRegDesc(i: Int, j: Int, wide: Int) = {
    val low = if (j == 0) 1 else j * 8
    val high = low + wide - 1
    RegFieldDesc(
      name = s"enables_${j}",
      desc = s"Targets ${low}-${high}. Set bits to 1 if interrupt should be enabled.",
      group = Some(s"enables_${i}"),
      groupDesc = Some(s"Enable bits for each interrupt source for target $i. 1 bit for each interrupt source.")
    )
  }

  def thresholdRegDesc(i: Int) =
    RegFieldDesc(
      name = s"threshold_$i",
      desc = s"Interrupt & claim threshold for target $i. Maximum value is ${parameter.nPriorities}.",
      reset = if (parameter.nPriorities > 0) None else Some(1)
    )

}
