// See LICENSE.SiFive for license details.

package freechips.rocketchip.devices.debug

import chisel3._
import chisel3.util.{log2Up, DecoupledIO}
import chisel3.experimental.dataview.DataViewable
import chisel3.experimental.hierarchy.{instantiable, Instance, Instantiate}
import chisel3.experimental.{SerializableModule, SerializableModuleParameter}
import chisel3.probe.{define, Probe, ProbeValue}
import chisel3.properties.{AnyClassType, Class, Property}

// case class DebugCustomParams(
//   addrs: List[Int],
//   width: Int
// ) {
//   require (width % 8 == 0, s"Currently only support custom debug widths which are multiples of 8, not ${width}")
// }

// class DebugCustomSinkInterface(p: DebugCustomParams) extends Bundle {
//   val addr = Input(UInt(log2Up(p.addrs.foldLeft(0){_ max _}).W))
//   val data = Output(UInt(p.width.W))
//   val ready = Output(Bool())
//   val valid = Input(Bool())
// }

// @instantiable
// class DebugCustomSink(val parameter: DebugCustomParams)
//     extends FixedIORawModule(new DebugCustomSinkInterface(parameter))
//     with SerializableModule[DebugCustomSinkInterface]
//     with ImplicitClock
//     with ImplicitReset {
// }
