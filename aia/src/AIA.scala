// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2024 Jiuyang Liu <liu@jiuyang.me>

package org.chipsalliance.uncore.aia

import chisel3._
import chisel3.experimental.hierarchy.{instantiable, Instance, Instantiate}
import chisel3.experimental.{SerializableModule, SerializableModuleParameter}
import chisel3.probe.{define, Probe, ProbeValue}
import chisel3.properties.{AnyClassType, Class, Property}

object AIAParameter {
  implicit def rwP: upickle.default.ReadWriter[AIAParameter] =
    upickle.default.macroRW
}

/** Parameter of [[AIA]] */
case class AIAParameter(useAsyncReset: Boolean) extends SerializableModuleParameter

/** Verification IO of [[AIA]] */
class AIAProbe(parameter: AIAParameter) extends Bundle {}

/** Metadata of [[AIA]]. */
@instantiable
class AIAOM(parameter: AIAParameter) extends Class {
  val useAsyncReset: Property[Boolean] = IO(Output(Property[Boolean]()))
  useAsyncReset := Property(parameter.useAsyncReset)
}

/** Interface of [[AIA]]. */
class AIAInterface(parameter: AIAParameter) extends Bundle {
  val clock = Input(Clock())
  val reset = Input(if (parameter.useAsyncReset) AsyncReset() else Bool())
  val probe = Output(Probe(new AIAProbe(parameter), layers.Verification))
  val om = Output(Property[AnyClassType]())
}

/** Hardware Implementation of AIA */
@instantiable
class AIA(val parameter: AIAParameter)
    extends FixedIORawModule(new AIAInterface(parameter))
    with SerializableModule[AIAParameter]
    with ImplicitClock
    with ImplicitReset {
  override protected def implicitClock: Clock = io.clock

  override protected def implicitReset: Reset = io.reset

  // Assign Probe
  val probeWire: AIAProbe = Wire(new AIAProbe(parameter))
  define(io.probe, ProbeValue(probeWire))

  // Assign Metadata
  val omInstance: Instance[AIAOM] = Instantiate(new AIAOM(parameter))
  io.om := omInstance.getPropertyReference.asAnyClassType
}
