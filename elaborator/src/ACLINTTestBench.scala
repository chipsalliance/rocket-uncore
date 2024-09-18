// SPDX-License-Identifier: Unlicense
// SPDX-FileCopyrightText: 2024 Jiuyang Liu <liu@jiuyang.me>
package org.chipsalliance.uncore.elaborator

import mainargs._
import org.chipsalliance.uncore.aclint.{ACLINTTestBench, ACLINTTestBenchParameter, TestVerbatimParameter}
import org.chipsalliance.uncore.elaborator.ACLINTMain.ACLINTParameterMain
import org.chipsalliance.uncore.elaborator.SerializableModuleElaborator

object ACLINTTestBenchMain extends SerializableModuleElaborator {
  implicit object PathRead extends TokensReader.Simple[os.Path] {
    def shortName = "path"
    def read(strs: Seq[String]) = Right(os.Path(strs.head, os.pwd))
  }

  @main
  case class ACLINTTestBenchParameterMain(
    @arg(name = "testVerbatimParameter") testVerbatimParameter: TestVerbatimParameterMain,
    @arg(name = "aclintParameter") aclintParameter:             ACLINTParameterMain,
    @arg(name = "timeout") timeout:                             Int,
    @arg(name = "testSize") testSize:                           Int) {
    def convert: ACLINTTestBenchParameter = ACLINTTestBenchParameter(
      testVerbatimParameter.convert,
      aclintParameter.convert,
      timeout,
      testSize
    )
  }

  case class TestVerbatimParameterMain(
    @arg(name = "useAsyncReset") useAsyncReset:       Boolean,
    @arg(name = "initFunctionName") initFunctionName: String,
    @arg(name = "dumpFunctionName") dumpFunctionName: String,
    @arg(name = "clockFlipTick") clockFlipTick:       Int,
    @arg(name = "resetFlipTick") resetFlipTick:       Int) {
    def convert: TestVerbatimParameter = TestVerbatimParameter(
      useAsyncReset:    Boolean,
      initFunctionName: String,
      dumpFunctionName: String,
      clockFlipTick:    Int,
      resetFlipTick:    Int
    )
  }

  implicit def TestVerbatimParameterMainParser: ParserForClass[TestVerbatimParameterMain] =
    ParserForClass[TestVerbatimParameterMain]

  implicit def ACLINTParameterMainParser: ParserForClass[ACLINTParameterMain] =
    ParserForClass[ACLINTParameterMain]

  implicit def ACLINTTestBenchParameterMainParser: ParserForClass[ACLINTTestBenchParameterMain] =
    ParserForClass[ACLINTTestBenchParameterMain]

  @main
  def config(
    @arg(name = "parameter") parameter:  ACLINTTestBenchParameterMain,
    @arg(name = "target-dir") targetDir: os.Path = os.pwd
  ) =
    os.write.over(targetDir / s"${getClass.getSimpleName.replace("$", "")}.json", configImpl(parameter.convert))

  @main
  def design(
    @arg(name = "parameter") parameter:  os.Path,
    @arg(name = "target-dir") targetDir: os.Path = os.pwd
  ) = {
    val design = designImpl[ACLINTTestBench, ACLINTTestBenchParameter](os.read(parameter))
    os.write.over(targetDir / s"${design.fir.main}.fir", design.firFile)
    os.write.over(targetDir / s"${design.fir.main}.anno.json", design.annosFile)
  }

  def main(args: Array[String]): Unit = ParserForMethods(this).runOrExit(args)
}
