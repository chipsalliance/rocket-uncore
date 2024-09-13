// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2024 Jiuyang Liu <liu@jiuyang.me>

package org.chipsalliance.sailtile

import chisel3._
import chisel3.experimental.hierarchy.{Instance, Instantiate, instantiable}
import chisel3.experimental.{SerializableModule, SerializableModuleParameter}
import chisel3.experimental.dataview.DataViewable
import chisel3.probe.{Probe, ProbeValue, define}
import chisel3.properties.{AnyClassType, Class, Property}
import chisel3.util.circt.dpi.RawClockedVoidFunctionCall
import org.chipsalliance.amba.axi4.bundle._

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
  RawClockedVoidFunctionCall("sail_reset")(io.clock, io.reset, io.hartid, io.resetVector)

  // SoC step
  // (hartId, Interrupt, Memory Respond(flit)) -> (Memory Request(flit), Trace)
  val loadStoreAXI = io.loadStoreAXI.viewAs[AXI4RWIrrevocableVerilog]
  val loadStoreAgent = Module(
    new AXI4MasterAgent(
      AXI4MasterAgentParameter(
        name = "loadStoreAXI",
        axiParameter = parameter.loadStoreParameter,
        outstanding = 4,
        readPayloadSize = 8,
        writePayloadSize = 8
      )
    ).suggestName("axi4_channel0_loadStoreAXI")
  )
  loadStoreAgent.io.channel match {
    case io: AXI4RWIrrevocableVerilog => io <> loadStoreAXI
  }
  loadStoreAgent.io.clock := io.clock
  loadStoreAgent.io.reset := io.reset
  loadStoreAgent.io.channelId := 0.U
  loadStoreAgent.io.gateRead := false.B
  loadStoreAgent.io.gateWrite := false.B

  val instructionFetchAXI = io.instructionFetchAXI.viewAs[AXI4ROIrrevocableVerilog]
  val instructionFetchAgent = Module(
    new AXI4MasterAgent(
      AXI4MasterAgentParameter(
        name = "instructionFetchAXI",
        axiParameter = parameter.instructionFetchParameter,
        outstanding = 4,
        readPayloadSize = 8,
        writePayloadSize = 8
      )
    ).suggestName("axi4_channel1_instructionFetchAXI")
  )
  instructionFetchAgent.io.channel match {
    case io: AXI4ROIrrevocableVerilog => io <> instructionFetchAXI
  }
  loadStoreAgent.io.clock := io.clock
  loadStoreAgent.io.reset := io.reset
  loadStoreAgent.io.channelId := 1.U
  loadStoreAgent.io.gateRead := false.B
  loadStoreAgent.io.gateWrite := false.B
}
