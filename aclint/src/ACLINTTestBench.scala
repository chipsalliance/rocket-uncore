// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2024 Jiuyang Liu <liu@jiuyang.me>

package org.chipsalliance.uncore.aclint

import chisel3._
import chisel3.experimental.hierarchy.{instantiable, public, Instance, Instantiate}
import chisel3.experimental.{SerializableModule, SerializableModuleParameter}
import chisel3.properties.{AnyClassType, Class, Property}
import chisel3.util.circt.dpi.RawUnclockedNonVoidFunctionCall
import chisel3.util.{Counter, HasExtModuleInline}

object ACLINTTestBenchParameter {
  implicit def rwP: upickle.default.ReadWriter[ACLINTTestBenchParameter] =
    upickle.default.macroRW
}

/** Parameter of [[ACLINT]]. */
case class ACLINTTestBenchParameter(
  testVerbatimParameter: TestVerbatimParameter,
  aclintParameter:       ACLINTParameter,
  timeout:               Int,
  testSize:              Int)
    extends SerializableModuleParameter {
  require(
    (testVerbatimParameter.useAsyncReset && aclintParameter.useAsyncReset) ||
      (!testVerbatimParameter.useAsyncReset && !aclintParameter.useAsyncReset),
    "Reset Type check failed."
  )
}

@instantiable
class ACLINTTestBenchOM(parameter: ACLINTTestBenchParameter) extends Class {
  val aclint = IO(Output(Property[AnyClassType]()))
  @public
  val aclintIn = IO(Input(Property[AnyClassType]()))
  aclint := aclintIn
}

class ACLINTTestBenchInterface(parameter: ACLINTTestBenchParameter) extends Bundle {
  val om = Output(Property[AnyClassType]())
}

@instantiable
class ACLINTTestBench(val parameter: ACLINTTestBenchParameter)
    extends FixedIORawModule(new ACLINTTestBenchInterface(parameter))
    with SerializableModule[ACLINTTestBenchParameter]
    with ImplicitClock
    with ImplicitReset {
  override protected def implicitClock: Clock = verbatim.io.clock

  override protected def implicitReset: Reset = verbatim.io.reset

  // Instantiate Drivers
  val verbatim: Instance[TestVerbatim] = Instantiate(
    new TestVerbatim(parameter.testVerbatimParameter)
  )
  // Instantiate DUT.
  val dut: Instance[ACLINT] = Instantiate(new ACLINT(parameter.aclintParameter))
  // Instantiate OM
  val omInstance = Instantiate(new ACLINTTestBenchOM(parameter))
  io.om := omInstance.getPropertyReference.asAnyClassType
  omInstance.aclintIn := dut.io.om

  dut.io.clock := implicitClock
  dut.io.reset := implicitReset

  // Simulation Logic
  val simulationTime: UInt = RegInit(0.U(64.W))
  simulationTime := simulationTime + 1.U
  // For each timeout ticks, check it
  val (_, callWatchdog) = Counter(true.B, parameter.timeout / 2)
  val watchdogCode = RawUnclockedNonVoidFunctionCall("aclint_watchdog", UInt(8.W))(callWatchdog)
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
  val aclint = IO(Output(Property[AnyClassType]()))
  @public
  val aclintIn = IO(Input(Property[AnyClassType]()))
  aclint := aclintIn
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
