// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2024 Jiuyang Liu <liu@jiuyang.me>

package org.chipsalliance.uncore.dm

import chisel3._
import chisel3.experimental.hierarchy.{instantiable, Instance, Instantiate}
import chisel3.experimental.{SerializableModule, SerializableModuleParameter}
import chisel3.probe.{define, Probe, ProbeValue}
import chisel3.properties.{AnyClassType, Class, Property}

object DMParameter {
  implicit def rwP: upickle.default.ReadWriter[DMParameter] =
    upickle.default.macroRW
}

/** Parameter of [[DM]] */
case class DMParameter(useAsyncReset: Boolean) extends SerializableModuleParameter

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
  val clock = Input(Clock())
  val reset = Input(if (parameter.useAsyncReset) AsyncReset() else Bool())
  val probe = Output(Probe(new DMProbe(parameter), layers.Verification))
  val om = Output(Property[AnyClassType]())
}

/** Hardware Implementation of DM */
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
}
