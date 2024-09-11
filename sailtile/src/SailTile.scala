// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2024 Jiuyang Liu <liu@jiuyang.me>

package org.chipsalliance.sailtile

import chisel3._
import chisel3.experimental.hierarchy.{Instance, Instantiate, instantiable}
import chisel3.experimental.{SerializableModule, SerializableModuleParameter}
import chisel3.probe.{Probe, ProbeValue, define}
import chisel3.properties.{AnyClassType, Class, Property}
import org.chipsalliance.amba.axi4.bundle.{AXI4BundleParameter, AXI4ROIrrevocable, AXI4RWIrrevocable}

object SailAXI4TileParameter {
  implicit def rwP: upickle.default.ReadWriter[SailAXI4TileParameter] =
    upickle.default.macroRW
}

/** Parameter of [[SailAXI4Tile]] */
case class SailAXI4TileParameter() extends SerializableModuleParameter {
  val hartIdLen:                 Int = 32
  val resetVectorBits:           Int = 64
  val instructionFetchParameter: AXI4BundleParameter = ???
  val loadStoreParameter:     AXI4BundleParameter = ???
}

/** Verification IO of [[SailAXI4Tile]] */
class SailAXI4TileProbe(parameter: SailAXI4TileParameter) extends Bundle {}

/** Metadata of [[SailAXI4Tile]]. */
@instantiable
class SailAXI4TileOM(parameter: SailAXI4TileParameter) extends Class {}

/** Interface of [[SailAXI4Tile]]. */
class SailAXI4TileInterface(parameter: SailAXI4TileParameter) extends Bundle {
  // coming from ClockGen
  val clock = Input(Clock())
  val reset = Input(Bool())

  val hartid = Flipped(UInt(parameter.hartIdLen.W))
  val resetVector = Input(UInt(parameter.resetVectorBits.W))
  // interrupt
  val debug: Bool = Input(Bool())
  val mtip:  Bool = Input(Bool())
  val msip:  Bool = Input(Bool())
  val meip:  Bool = Input(Bool())
  val seip:  Bool = Input(Bool())
  // memory interface
  val instructionFetchAXI: AXI4ROIrrevocable = org.chipsalliance.amba.axi4.bundle.AXI4ROIrrevocable(parameter.instructionFetchParameter)
  val loadStoreAXI: AXI4RWIrrevocable = org.chipsalliance.amba.axi4.bundle.AXI4RWIrrevocable(parameter.loadStoreParameter)
  val probe = Output(Probe(new SailAXI4TileProbe(parameter), layers.Verification))
  val om = Output(Property[AnyClassType]())
}

/** Hardware Implementation of SailAXI4Tile */
@instantiable
class SailAXI4Tile(val parameter: SailAXI4TileParameter)
    extends FixedIORawModule(new SailAXI4TileInterface(parameter))
    with SerializableModule[SailAXI4TileParameter]
    with ImplicitClock
    with ImplicitReset {
  override protected def implicitClock: Clock = io.clock

  override protected def implicitReset: Reset = io.reset

  // Assign Probe
  val probeWire: SailAXI4TileProbe = Wire(new SailAXI4TileProbe(parameter))
  define(io.probe, ProbeValue(probeWire))

  // Assign Metadata
  val omInstance: Instance[SailAXI4TileOM] = Instantiate(new SailAXI4TileOM(parameter))
  io.om := omInstance.getPropertyReference.asAnyClassType

  // Reset
  // @reset (hartId, resetVector) -> ()
  // SoC step
  // (hartId, Interrupt, Memory Respond(flit)) -> (Memory Request(flit), Trace)
}
