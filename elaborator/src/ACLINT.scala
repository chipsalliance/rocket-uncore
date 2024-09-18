// SPDX-License-Identifier: Unlicense
// SPDX-FileCopyrightText: 2024 Jiuyang Liu <liu@jiuyang.me>
package org.chipsalliance.uncore.elaborator

import mainargs._
import org.chipsalliance.amba.axi4.bundle.AXI4BundleParameter
import org.chipsalliance.uncore.aclint.{ACLINT, ACLINTParameter}
import org.chipsalliance.uncore.elaborator.SerializableModuleElaborator

object ACLINTMain extends SerializableModuleElaborator {
  implicit object PathRead extends TokensReader.Simple[os.Path] {
    def shortName = "path"
    def read(strs: Seq[String]) = Right(os.Path(strs.head, os.pwd))
  }

  implicit object BigIntRead extends TokensReader.Simple[BigInt] {
    def shortName = "bigint"
    def read(strs: Seq[String]) = Right(BigInt(strs.last))
  }

  @main
  case class AXI4BundleParameterMain(
    @arg(name = "idWidth") idWidth:     Int,
    @arg(name = "dataWidth") dataWidth: Int,
    @arg(name = "addrWidth") addrWidth: Int) {
    def convert: AXI4BundleParameter = AXI4BundleParameter(
      idWidth,
      dataWidth,
      addrWidth,
      userReqWidth = 0,
      userDataWidth = 0,
      userRespWidth = 0,
      hasAW = true,
      hasW = true,
      hasB = true,
      hasAR = true,
      hasR = true,
      supportId = true,
      supportRegion = false,
      supportLen = true,
      supportSize = true,
      supportBurst = true,
      supportLock = false,
      supportCache = false,
      supportQos = false,
      supportStrb = true,
      supportResp = false,
      supportProt = false
    )
  }

  implicit def AXI4BundleParameterMainParser: ParserForClass[AXI4BundleParameterMain] =
    ParserForClass[AXI4BundleParameterMain]

  @main
  case class ACLINTParameterMain(
    @arg(name = "useAsyncReset") useAsyncReset: Boolean,
    // FIXME: BigInt
    @arg(name = "mtimeBase") mtimeBase:           Int,
    @arg(name = "mtimecmpBase") mtimecmpBase:     Int,
    @arg(name = "harts") harts:                   Int,
    @arg(name = "satelliteMtime") satelliteMtime: Boolean,
    @arg(name = "axi4Parameter") axi4Parameter:   AXI4BundleParameterMain) {

    def convert: ACLINTParameter =
      ACLINTParameter(useAsyncReset, mtimeBase, mtimecmpBase, harts, satelliteMtime, axi4Parameter.convert)
  }

  object ACLINTParameterMain {}

  implicit def ACLINTParameterMainParser: ParserForClass[ACLINTParameterMain] =
    ParserForClass[ACLINTParameterMain]

  @main
  def config(
    @arg(name = "parameter") parameter:  ACLINTParameterMain,
    @arg(name = "target-dir") targetDir: os.Path = os.pwd
  ) =
    os.write.over(targetDir / s"${getClass.getSimpleName.replace("$", "")}.json", configImpl(parameter.convert))

  @main
  def design(
    @arg(name = "parameter") parameter:  os.Path,
    @arg(name = "target-dir") targetDir: os.Path = os.pwd
  ) = {
    val design = designImpl[ACLINT, ACLINTParameter](os.read(parameter))
    os.write.over(targetDir / s"${design.fir.main}.fir", design.firFile)
    os.write.over(targetDir / s"${design.fir.main}.anno.json", design.annosFile)
  }

  def main(args: Array[String]): Unit = ParserForMethods(this).runOrExit(args)
}
