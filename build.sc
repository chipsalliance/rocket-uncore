// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2024 Jiuyang Liu <liu@jiuyang.me>

import mill._
import mill.scalalib._
import mill.define.{Command, TaskModule}
import mill.scalalib.publish._
import mill.scalalib.scalafmt._
import mill.scalalib.TestModule.Utest
import mill.util.Jvm
import coursier.maven.MavenRepository
import $file.dependencies.chisel.build
import $file.dependencies.`chisel-interface`.common
import $file.common

object v {
  val scala = "2.13.14"
  val mainargs = ivy"com.lihaoyi::mainargs:0.5.0"
  val oslib = ivy"com.lihaoyi::os-lib:0.9.1"
  val upickle = ivy"com.lihaoyi::upickle:3.3.1"
}

object chisel extends Chisel

trait Chisel extends millbuild.dependencies.chisel.build.Chisel {
  def crossValue = v.scala
  override def millSourcePath = os.pwd / "dependencies" / "chisel"
}

object axi4 extends AXI4

trait AXI4 extends millbuild.dependencies.`chisel-interface`.common.AXI4Module {
  override def millSourcePath = os.pwd / "dependencies" / "chisel-interface" / "axi4"
  def scalaVersion            = v.scala

  def mainargsIvy = v.mainargs

  def chiselModule    = Some(chisel)
  def chiselPluginJar = T(Some(chisel.pluginModule.jar()))
  def chiselIvy       = None
  def chiselPluginIvy = None
}

// API to be upstreamed to axi4
object regrouter extends millbuild.common.HasChisel with ScalafmtModule {
  def scalaVersion = T(v.scala)
  override def moduleDeps = super.moduleDeps ++ Seq(axi4)

  def chiselModule = Some(chisel)
  def chiselPluginJar = T(Some(chisel.pluginModule.jar()))
  def chiselIvy = None
  def chiselPluginIvy = None
}

object dwbb extends DWBB

trait DWBB extends millbuild.dependencies.`chisel-interface`.common.DWBBModule {
  override def millSourcePath = os.pwd / "dependencies" / "chisel-interface" / "dwbb"
  def scalaVersion            = v.scala

  def mainargsIvy = v.mainargs

  def chiselModule    = Some(chisel)
  def chiselPluginJar = T(Some(chisel.pluginModule.jar()))
  def chiselIvy       = None
  def chiselPluginIvy = None
}

object gcd extends millbuild.common.HasChisel with ScalafmtModule {
  def scalaVersion = T(v.scala)

  def chiselModule = Some(chisel)
  def chiselPluginJar = T(Some(chisel.pluginModule.jar()))
  def chiselIvy = None
  def chiselPluginIvy = None
}

/** [[https://github.com/riscv/riscv-aclint]] */
object aclint extends ACLINT
trait ACLINT extends millbuild.common.ACLINTModule with ScalafmtModule {
  def scalaVersion = T(v.scala)

  def axi4Module: ScalaModule = axi4
  def dwbbModule: ScalaModule = dwbb

  def chiselModule = Some(chisel)
  def chiselPluginJar = T(Some(chisel.pluginModule.jar()))
  def chiselIvy = None
  def chiselPluginIvy = None
  override def moduleDeps = super.moduleDeps ++ Seq(regrouter)
}

/** [[https://github.com/riscv/riscv-plic-spec]] */
object plic extends PLIC
trait PLIC extends millbuild.common.PLICModule with ScalafmtModule {
  def scalaVersion = T(v.scala)

  def axi4Module: ScalaModule = axi4
  def dwbbModule: ScalaModule = dwbb

  def chiselModule = Some(chisel)
  def chiselPluginJar = T(Some(chisel.pluginModule.jar()))
  def chiselIvy = None
  def chiselPluginIvy = None
  override def moduleDeps = super.moduleDeps ++ Seq(regrouter)
}

/** [[https://github.com/riscv/riscv-debug-spec]] */
object dm extends DM
trait DM extends millbuild.common.DMModule with ScalafmtModule {
  def scalaVersion = T(v.scala)

  def axi4Module: ScalaModule = axi4
  def dwbbModule: ScalaModule = dwbb

  def chiselModule = Some(chisel)
  def chiselPluginJar = T(Some(chisel.pluginModule.jar()))
  def chiselIvy = None
  def chiselPluginIvy = None
  override def moduleDeps = super.moduleDeps ++ Seq(regrouter)
}

/** [[https://github.com/riscv/riscv-fast-interrupt]] */
object clic extends CLIC
trait CLIC extends millbuild.common.CLICModule with ScalafmtModule {
  def scalaVersion = T(v.scala)

  def axi4Module: ScalaModule = axi4
  def dwbbModule: ScalaModule = dwbb

  def chiselModule = Some(chisel)
  def chiselPluginJar = T(Some(chisel.pluginModule.jar()))
  def chiselIvy = None
  def chiselPluginIvy = None
  override def moduleDeps = super.moduleDeps ++ Seq(regrouter)
}

/** [[https://github.com/riscv/riscv-aia]] */
object aia extends AIA
trait AIA extends millbuild.common.AIAModule with ScalafmtModule {
  def scalaVersion = T(v.scala)

  def axi4Module: ScalaModule = axi4
  def dwbbModule: ScalaModule = dwbb

  def chiselModule = Some(chisel)
  def chiselPluginJar = T(Some(chisel.pluginModule.jar()))
  def chiselIvy = None
  def chiselPluginIvy = None
  override def moduleDeps = super.moduleDeps ++ Seq(regrouter)
}

/** [[https://github.com/riscv/riscv-aia]] */
object iommu extends IOMMU
trait IOMMU extends millbuild.common.IOMMUModule with ScalafmtModule {
  def scalaVersion = T(v.scala)

  def axi4Module: ScalaModule = axi4
  def dwbbModule: ScalaModule = dwbb

  def chiselModule = Some(chisel)
  def chiselPluginJar = T(Some(chisel.pluginModule.jar()))
  def chiselIvy = None
  def chiselPluginIvy = None
  override def moduleDeps = super.moduleDeps ++ Seq(regrouter)
}

object elaborator extends Elaborator
trait Elaborator extends millbuild.common.ElaboratorModule {
  def scalaVersion = T(v.scala)

  def panamaconverterModule = panamaconverter

  def circtInstallPath =
    T.input(PathRef(os.Path(T.ctx().env("CIRCT_INSTALL_PATH"))))

  def generators = Seq(gcd, aclint, plic, dm, clic, aia, iommu)

  def mainargsIvy = v.mainargs

  def chiselModule = Some(chisel)
  def chiselPluginJar = T(Some(chisel.pluginModule.jar()))
  def chiselPluginIvy = None
  def chiselIvy = None
  override def moduleDeps = super.moduleDeps ++ Seq(regrouter)
}

object panamaconverter extends PanamaConverter
trait PanamaConverter extends millbuild.dependencies.chisel.build.PanamaConverter {
  def crossValue = v.scala

  override def millSourcePath =
    os.pwd / "dependencies" / "chisel" / "panamaconverter"

  def scalaVersion = T(v.scala)
}
