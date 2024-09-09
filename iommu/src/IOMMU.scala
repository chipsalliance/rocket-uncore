// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2024 Jiuyang Liu <liu@jiuyang.me>

package org.chipsalliance.uncore.iommu

import chisel3._
import chisel3.experimental.hierarchy.{instantiable, Instance, Instantiate}
import chisel3.experimental.{SerializableModule, SerializableModuleParameter}
import chisel3.probe.{define, Probe, ProbeValue}
import chisel3.properties.{AnyClassType, Class, Property}

object IOMMUParameter {
  implicit def rwP: upickle.default.ReadWriter[IOMMUParameter] =
    upickle.default.macroRW
}

/** Parameter of [[IOMMU]] */
case class IOMMUParameter(useAsyncReset: Boolean) extends SerializableModuleParameter

/** Verification IO of [[IOMMU]] */
class IOMMUProbe(parameter: IOMMUParameter) extends Bundle {}

/** Metadata of [[IOMMU]]. */
@instantiable
class IOMMUOM(parameter: IOMMUParameter) extends Class {
  val useAsyncReset: Property[Boolean] = IO(Output(Property[Boolean]()))
  useAsyncReset := Property(parameter.useAsyncReset)
}

/** Interface of [[IOMMU]]. */
class IOMMUInterface(parameter: IOMMUParameter) extends Bundle {
  val clock = Input(Clock())
  val reset = Input(if (parameter.useAsyncReset) AsyncReset() else Bool())
  val probe = Output(Probe(new IOMMUProbe(parameter), layers.Verification))
  val om = Output(Property[AnyClassType]())
}

/** Hardware Implementation of IOMMU */
@instantiable
class IOMMU(val parameter: IOMMUParameter)
    extends FixedIORawModule(new IOMMUInterface(parameter))
    with SerializableModule[IOMMUParameter]
    with ImplicitClock
    with ImplicitReset {
  override protected def implicitClock: Clock = io.clock

  override protected def implicitReset: Reset = io.reset

  // Assign Probe
  val probeWire: IOMMUProbe = Wire(new IOMMUProbe(parameter))
  define(io.probe, ProbeValue(probeWire))

  // Assign Metadata
  val omInstance: Instance[IOMMUOM] = Instantiate(new IOMMUOM(parameter))
  io.om := omInstance.getPropertyReference.asAnyClassType
}

object IOMMUTestBenchParameter {
  implicit def rwP: upickle.default.ReadWriter[IOMMUTestBenchParameter] =
    upickle.default.macroRW
}