// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2024 Jiuyang Liu <liu@jiuyang.me>

package org.chipsalliance.uncore.plic

import chisel3._
import chisel3.experimental.hierarchy.{instantiable, Instance, Instantiate}
import chisel3.experimental.{SerializableModule, SerializableModuleParameter}
import chisel3.probe.{define, Probe, ProbeValue}
import chisel3.properties.{AnyClassType, Class, Property}

object PLICParameter {
  implicit def rwP: upickle.default.ReadWriter[PLICParameter] =
    upickle.default.macroRW
}

/** Parameter of [[PLIC]] */
case class PLICParameter(useAsyncReset: Boolean) extends SerializableModuleParameter

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
}

object PLICTestBenchParameter {
  implicit def rwP: upickle.default.ReadWriter[PLICTestBenchParameter] =
    upickle.default.macroRW
}
