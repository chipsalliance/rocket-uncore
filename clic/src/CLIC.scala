// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2024 Jiuyang Liu <liu@jiuyang.me>

package org.chipsalliance.uncore.clic

import chisel3._
import chisel3.experimental.hierarchy.{instantiable, Instance, Instantiate}
import chisel3.experimental.{SerializableModule, SerializableModuleParameter}
import chisel3.probe.{define, Probe, ProbeValue}
import chisel3.properties.{AnyClassType, Class, Property}

object CLICParameter {
  implicit def rwP: upickle.default.ReadWriter[CLICParameter] =
    upickle.default.macroRW
}

/** Parameter of [[CLIC]] */
case class CLICParameter(useAsyncReset: Boolean) extends SerializableModuleParameter

/** Verification IO of [[CLIC]] */
class CLICProbe(parameter: CLICParameter) extends Bundle {}

/** Metadata of [[CLIC]]. */
@instantiable
class CLICOM(parameter: CLICParameter) extends Class {
  val useAsyncReset: Property[Boolean] = IO(Output(Property[Boolean]()))
  useAsyncReset := Property(parameter.useAsyncReset)
}

/** Interface of [[CLIC]]. */
class CLICInterface(parameter: CLICParameter) extends Bundle {
  val clock = Input(Clock())
  val reset = Input(if (parameter.useAsyncReset) AsyncReset() else Bool())
  val probe = Output(Probe(new CLICProbe(parameter), layers.Verification))
  val om = Output(Property[AnyClassType]())
}

/** Hardware Implementation of CLIC */
@instantiable
class CLIC(val parameter: CLICParameter)
    extends FixedIORawModule(new CLICInterface(parameter))
    with SerializableModule[CLICParameter]
    with ImplicitClock
    with ImplicitReset {
  override protected def implicitClock: Clock = io.clock

  override protected def implicitReset: Reset = io.reset

  // Assign Probe
  val probeWire: CLICProbe = Wire(new CLICProbe(parameter))
  define(io.probe, ProbeValue(probeWire))

  // Assign Metadata
  val omInstance: Instance[CLICOM] = Instantiate(new CLICOM(parameter))
  io.om := omInstance.getPropertyReference.asAnyClassType
}
