// SPDX-License-Identifier: Unlicense
// SPDX-FileCopyrightText: 2024 Jiuyang Liu <liu@jiuyang.me>
package org.chipsalliance.uncore.elaborator

import chisel3.RawModule
import chisel3.experimental.{SerializableModule, SerializableModuleGenerator, SerializableModuleParameter}

import scala.reflect.runtime.universe
import scala.reflect.runtime.universe.{runtimeMirror, typeOf}

case class SerializableModuleDesign(fir: firrtl.ir.Circuit, annos: Seq[firrtl.annotations.Annotation]) {
  def firFile = fir.serialize
  def annosFile = firrtl.annotations.JsonProtocol.serializeRecover(annos)
}

trait SerializableModuleElaborator {
  def configImpl[P <: SerializableModuleParameter: universe.TypeTag](
    parameter: P
  )(
    implicit rwP: upickle.default.Writer[P]
  ) = upickle.default.write(parameter)

  def designImpl[M <: SerializableModule[P]: universe.TypeTag, P <: SerializableModuleParameter: universe.TypeTag](
    parameter: String
  )(
    implicit
    rwP: upickle.default.Reader[P]
  ): SerializableModuleDesign = {
    var fir: firrtl.ir.Circuit = null
    val annos = Seq(
      new chisel3.stage.phases.Elaborate,
      new chisel3.stage.phases.Convert
    ).foldLeft(
      Seq(
        chisel3.stage.ChiselGeneratorAnnotation(() =>
          SerializableModuleGenerator(
            runtimeMirror(getClass.getClassLoader)
              .runtimeClass(typeOf[M].typeSymbol.asClass)
              .asInstanceOf[Class[M]],
            upickle.default.read[P](parameter)
          ).module().asInstanceOf[RawModule]
        )
      ): firrtl.AnnotationSeq
    ) { case (annos, stage) => stage.transform(annos) }
      .flatMap {
        case firrtl.stage.FirrtlCircuitAnnotation(circuit) =>
          fir = circuit
          None
        case _: firrtl.options.Unserializable => None
        case a => Some(a)
      }
    SerializableModuleDesign(fir, annos)
  }
}
