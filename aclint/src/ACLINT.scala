// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2024 Jiuyang Liu <liu@jiuyang.me>

package org.chipsalliance.uncore.aclint

import chisel3._
import chisel3.experimental.hierarchy.{instantiable, Instance, Instantiate}
import chisel3.experimental.{SerializableModule, SerializableModuleParameter}
import chisel3.probe.{define, Probe, ProbeValue}
import chisel3.properties.{AnyClassType, Class, Property}

object ACLINTParameter {
  implicit def rwP: upickle.default.ReadWriter[ACLINTParameter] =
    upickle.default.macroRW
}

/** Parameter of [[ACLINT]] */
case class ACLINTParameter(useAsyncReset: Boolean) extends SerializableModuleParameter

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
}
