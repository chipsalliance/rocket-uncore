// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2024 Jiuyang Liu <liu@jiuyang.me>

package org.chipsalliance.uncore.plic

import chisel3._
import chisel3.experimental.hierarchy.{instantiable, public, Instance, Instantiate}
import chisel3.experimental.{SerializableModule, SerializableModuleParameter}
import chisel3.properties.{AnyClassType, Class, Property}
import chisel3.util.circt.dpi.RawUnclockedNonVoidFunctionCall
import chisel3.util.{Counter, HasExtModuleInline}

/** Parameter of [[PLIC]]. */
case class PLICTestBenchParameter(
  testVerbatimParameter: TestVerbatimParameter,
  plicParameter:         PLICParameter,
  timeout:               Int,
  testSize:              Int)
    extends SerializableModuleParameter {
  require(
    (testVerbatimParameter.useAsyncReset && plicParameter.useAsyncReset) ||
      (!testVerbatimParameter.useAsyncReset && !plicParameter.useAsyncReset),
    "Reset Type check failed."
  )
}

@instantiable
class PLICTestBenchOM(parameter: PLICTestBenchParameter) extends Class {
  val plic = IO(Output(Property[AnyClassType]()))
  @public
  val plicIn = IO(Input(Property[AnyClassType]()))
  plic := plicIn
}

class PLICTestBenchInterface(parameter: PLICTestBenchParameter) extends Bundle {
  val om = Output(Property[AnyClassType]())
}

@instantiable
class PLICTestBench(val parameter: PLICTestBenchParameter)
    extends FixedIORawModule(new PLICTestBenchInterface(parameter))
    with SerializableModule[PLICTestBenchParameter]
    with ImplicitClock
    with ImplicitReset {
  override protected def implicitClock: Clock = verbatim.io.clock

  override protected def implicitReset: Reset = verbatim.io.reset

  // Instantiate Drivers
  val verbatim: Instance[TestVerbatim] = Instantiate(
    new TestVerbatim(parameter.testVerbatimParameter)
  )
  // Instantiate DUT.
  val dut: Instance[PLIC] = Instantiate(new PLIC(parameter.plicParameter))
  // Instantiate OM
  val omInstance = Instantiate(new PLICTestBenchOM(parameter))
  io.om := omInstance.getPropertyReference.asAnyClassType
  omInstance.plicIn := dut.io.om

  dut.io.clock := implicitClock
  dut.io.reset := implicitReset

  // Simulation Logic
  val simulationTime: UInt = RegInit(0.U(64.W))
  simulationTime := simulationTime + 1.U
  // For each timeout ticks, check it
  val (_, callWatchdog) = Counter(true.B, parameter.timeout / 2)
  val watchdogCode = RawUnclockedNonVoidFunctionCall("plic_watchdog", UInt(8.W))(callWatchdog)
  when(watchdogCode =/= 0.U) {
    stop(cf"""{"event":"SimulationStop","reason": ${watchdogCode},"cycle":${simulationTime}}\n""")
  }
}

object TestVerbatimParameter {
  implicit def rwP: upickle.default.ReadWriter[TestVerbatimParameter] =
    upickle.default.macroRW
}

case class TestVerbatimParameter(
  useAsyncReset:    Boolean,
  initFunctionName: String,
  dumpFunctionName: String,
  clockFlipTick:    Int,
  resetFlipTick:    Int)
    extends SerializableModuleParameter

@instantiable
class TestVerbatimOM(parameter: TestVerbatimParameter) extends Class {
  val useAsyncReset:    Property[Boolean] = IO(Output(Property[Boolean]()))
  val initFunctionName: Property[String] = IO(Output(Property[String]()))
  val dumpFunctionName: Property[String] = IO(Output(Property[String]()))
  val clockFlipTick:    Property[Int] = IO(Output(Property[Int]()))
  val resetFlipTick:    Property[Int] = IO(Output(Property[Int]()))
  val plic = IO(Output(Property[AnyClassType]()))
  @public
  val plicIn = IO(Input(Property[AnyClassType]()))
  plic := plicIn
  useAsyncReset := Property(parameter.useAsyncReset)
  initFunctionName := Property(parameter.initFunctionName)
  dumpFunctionName := Property(parameter.dumpFunctionName)
  clockFlipTick := Property(parameter.clockFlipTick)
  resetFlipTick := Property(parameter.resetFlipTick)
}

/** Test blackbox for clockgen, wave dump and extra testbench-only codes. */
class TestVerbatimInterface(parameter: TestVerbatimParameter) extends Bundle {
  val clock: Clock = Output(Clock())
  val reset: Reset = Output(
    if (parameter.useAsyncReset) AsyncReset() else Bool()
  )
}

@instantiable
class TestVerbatim(parameter: TestVerbatimParameter)
    extends FixedIOExtModule(new TestVerbatimInterface(parameter))
    with HasExtModuleInline {
  setInline(
    s"$desiredName.sv",
    s"""module $desiredName(output reg clock, output reg reset);
       |  export "DPI-C" function ${parameter.dumpFunctionName};
       |  function ${parameter.dumpFunctionName}(input string file);
       |`ifdef VCS
       |    $$fsdbDumpfile(file);
       |    $$fsdbDumpvars("+all");
       |    $$fsdbDumpSVA;
       |    $$fsdbDumpon;
       |`endif
       |`ifdef VERILATOR
       |    $$dumpfile(file);
       |    $$dumpvars(0);
       |`endif
       |  endfunction;
       |
       |  import "DPI-C" context function void ${parameter.initFunctionName}();
       |  initial begin
       |    ${parameter.initFunctionName}();
       |    clock = 1'b0;
       |    reset = 1'b1;
       |  end
       |  initial #(${parameter.resetFlipTick}) reset = 1'b0;
       |  always #${parameter.clockFlipTick} clock = ~clock;
       |endmodule
       |""".stripMargin
  )
}
