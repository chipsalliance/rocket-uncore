// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2016-2017 SiFive, Inc.
// SPDX-FileCopyrightText: 2024 Jiuyang Liu <liu@jiuyang.me>
// After finishing one or two riscv uncore ip developing, it will be upstreamed to chipsalliance/amba

package org.chipsalliance.amba

import chisel3._
import chisel3.util.{Decoupled, DecoupledIO, Irrevocable, ReadyValidIO}

import scala.language.implicitConversions
import scala.util.matching.Regex

// TODO: find some upickle friendly API?
object RegFieldAccessType extends scala.Enumeration {
  type RegFieldAccessType = Value
  val R, W, RW = Value
}
import org.chipsalliance.amba.RegFieldAccessType._

object RegFieldWrType extends scala.Enumeration {
  type RegFieldWrType = Value
  val ONE_TO_CLEAR, ONE_TO_SET, ONE_TO_TOGGLE, ZERO_TO_CLEAR,
  ZERO_TO_SET, ZERO_TO_TOGGLE, CLEAR, SET, MODIFY = Value
}
import org.chipsalliance.amba.RegFieldWrType._

object RegFieldRdAction extends scala.Enumeration {
  type RegFieldRdAction = Value
  val CLEAR, SET, MODIFY = Value
}
import org.chipsalliance.amba.RegFieldRdAction._

// A RegFieldDesc is a documentation-only metadata about a RegField.
// This is similar to the IP-XACT concept of a Field within a
// Register. Notably, RegMapper does not have a concept
// of a Register to match the IP-XACT concept.
// TODO: This should be a Class and expose to OM
case class RegFieldDesc(
                         // A short name of the register field, which
                         // could be expected to be used in macros/diagrams
                         name: String,
                         // A longer documentation of what the register
                         // field does.
                         desc: String,
                         // The "register" name that this register field can be
                         // considered to be a part of, in traditional ways of
                         // thinking about memory mapped control registers as being
                         // fixed in size. Generally expected to be used in macros/diagrams
                         group: Option[String] = None,
                         // A "register" description for the above group, a longer
                         // description of what the register does.
                         groupDesc: Option[String] = None,
                         // The general access type of the register field
                         access: RegFieldAccessType = RegFieldAccessType.RW,
                         // The special write type of the register field
                         wrType: Option[RegFieldWrType] = None,
                         // the special read side effects of the register field
                         rdAction: Option[RegFieldRdAction] = None,
                         // Whether this register field can change between reads without
                         // being accessed again by the bus interface
                         volatile: Boolean = false,
                         // TODO: testable?
                         // The reset value of this register, if it has one
                         reset: Option[BigInt] = None,
                         // Enumerated values that this register field can take
                         enumerations: Map[BigInt, (String, String)] = Map(),
                         // The IP-XACT concept of an addressBlock which this
                         // register field can consider to be a part of, if exporting
                         // IP-XACT or similar outputs.
                         addressBlock: Option[AddressBlockInfo] = None
                         // TODO: registerFiles
                       ) {
  require(RegFieldDesc.nameAcceptable(name),
    s"RegFieldDesc.name of '$name' is not of the form '${RegFieldDesc.nameRegex.toString}'")

  // We could also check for group name here, but that can have a
  // significant runtime overhead because every RegField can be
  // annotated with the group name,
  // so we put the check in the RegFieldGroup helper function instead.

}

object RegFieldDesc {
  def reserved: RegFieldDesc = RegFieldDesc("reserved", "", access = RegFieldAccessType.R, reset = Some(0))

  // This Regex is more limited than the IP-XACT standard,
  // which allows some Unicode characters as well.
  val nameRegex: Regex = """^[_:A-Za-z][-._:A-Za-z0-9]*$""".r

  def nameAcceptable(name: String): Boolean = name match {
    case RegFieldDesc.nameRegex(_*) => true
    case _ => false
  }
}

// Our descriptions are in terms of RegFields only, which is somewhat
// unusual for developers who are used to things being defined as bitfields
// within registers. The "Group" allows a string & (optional) description
// to be added which describes the conceptual "Group" the RegField belongs to.
// This can be used by downstream flows as they see fit to present the information.

object RegFieldGroup {
  def apply(name: String, desc: Option[String], regs: Seq[RegField], descFirstOnly: Boolean = true): Seq[RegField] = {
    require(RegFieldDesc.nameAcceptable(name),
      s"RegFieldDesc.group of '$name' is not of the form '${RegFieldDesc.nameRegex.toString}'")
    regs.zipWithIndex.map { case (r, i) =>
      val gDesc = if ((i > 0) & descFirstOnly) None else desc
      r.desc.map { d =>
        r.copy(desc = Some(d.copy(group = Some(name), groupDesc = gDesc)))
      }.getOrElse(r)
    }
  }
}

// The "AddressBlock" allows an optional AddressBlockInfo to be associated
// with a register field.
// This can be used by downstream flows as they see fit to present the
// information. This is generally designed to match the IP-XACT
// concept of an AddressBlock.

case class AddressBlockInfo(
                             // The short name of the address block
                             name: String,
                             addressOffset: BigInt, // Offset of the address block (in bytes) from the MemoryMap base / base of the Register Router node.
                             // This is generally NOT an absoluate address.
                             // Note this is NOT expected to be considered as part of RegField's offset:
                             // adding an AddressBlockInfo should not change that value in the RegField serialized to JSON or OM, for example.
                             range: BigInt, // Size of the address block (in bytes)
                             width: Int // assumed access size of registers in this block (e.g. 32 or 64 bits).
                             // Again this is ONLY documentation. RegField hardware generation ignores this.
                           );

// Add the AddressBlock to a list of RegFields' descriptions. If they have no RegFieldDesc,
// this has no effect.
object RegFieldAddressBlock {

  def apply(addressBlockInfo: AddressBlockInfo,
            addAddressOffset: Boolean,
            regmap: RegField.Map*): Seq[RegField.Map] = {
    regmap.toList.map { regmapEntry =>
      // each entry has a form like offset -> Seq[RegField]
      // We either add the addressBlockInfo.addressOffset or not
      // and also update each of the child RegField's descriptions
      val regFields = regmapEntry._2
      val regFieldsWithAddressBlockAdded = regFields.map { r =>
        r.desc.map { d =>
          r.copy(desc = Some(d.copy(addressBlock = Some(addressBlockInfo))))
        }.getOrElse(r)
      }
      val offsetIncrement = if (addAddressOffset) addressBlockInfo.addressOffset.toInt else 0
      (regmapEntry._1 + offsetIncrement) -> regFieldsWithAddressBlockAdded
    }
  }
}

// RegField should support connecting to one of these
class RegisterReadIO[T <: Data](gen: T) extends Bundle {
  val request = Flipped(Decoupled(Bool())) // ignore .bits
  val response = Irrevocable(gen)
}

// RegField should support connecting to one of these
class RegisterWriteIO[T <: Data](gen: T) extends Bundle {
  val request = Flipped(Decoupled(gen))
  val response = Irrevocable(Bool()) // ignore .bits
}


class SimpleRegIO(val w: Int) extends Bundle {
  val d = Input(UInt(w.W))
  val q = Output(UInt(w.W))
  val en = Input(Bool())
}


case class RegReadFn private(combinational: Boolean, fn: (Bool, Bool) => (Bool, Bool, UInt))

object RegReadFn {
  // (ivalid: Bool, oready: Bool) => (iready: Bool, ovalid: Bool, data: UInt)
  // iready may combinationally depend on oready
  // all other combinational dependencies forbidden (e.g. ovalid <= ivalid)
  // effects must become visible on the cycle after ovalid && oready
  // data is only inspected when ovalid && oready
  implicit def apply(x: (Bool, Bool) => (Bool, Bool, UInt)) =
    new RegReadFn(false, x)

  implicit def apply(x: RegisterReadIO[UInt]): RegReadFn =
    RegReadFn((ivalid, oready) => {
      x.request.valid := ivalid
      x.response.ready := oready
      (x.request.ready, x.response.valid, x.response.bits)
    })

  // (ready: Bool) => (valid: Bool, data: UInt)
  // valid must not combinationally depend on ready
  // effects must become visible on the cycle after valid && ready
  implicit def apply(x: Bool => (Bool, UInt)) =
    new RegReadFn(true, { case (_, oready) =>
      val (ovalid, data) = x(oready)
      (true.B, ovalid, data)
    })

  // read from a ReadyValidIO (only safe if there is a consistent source of data)
  implicit def apply(x: ReadyValidIO[UInt]): RegReadFn = RegReadFn(ready => {
    x.ready := ready;
    (x.valid, x.bits)
  })

  // read from a register
  implicit def apply(x: UInt): RegReadFn = RegReadFn(ready => (true.B, x))

  // noop
  implicit def apply(x: Unit): RegReadFn = RegReadFn(0.U)
}

case class RegWriteFn private(combinational: Boolean, fn: (Bool, Bool, UInt) => (Bool, Bool))

object RegWriteFn {
  // (ivalid: Bool, oready: Bool, data: UInt) => (iready: Bool, ovalid: Bool)
  // iready may combinationally depend on both oready and data
  // all other combinational dependencies forbidden (e.g. ovalid <= ivalid)
  // effects must become visible on the cycle after ovalid && oready
  // data should only be used for an effect when ivalid && iready
  implicit def apply(x: (Bool, Bool, UInt) => (Bool, Bool)) =
    new RegWriteFn(false, x)

  implicit def apply(x: RegisterWriteIO[UInt]): RegWriteFn =
    RegWriteFn((ivalid, oready, data) => {
      x.request.valid := ivalid
      x.request.bits := data
      x.response.ready := oready
      (x.request.ready, x.response.valid)
    })

  // (valid: Bool, data: UInt) => (ready: Bool)
  // ready may combinationally depend on data (but not valid)
  // effects must become visible on the cycle after valid && ready
  implicit def apply(x: (Bool, UInt) => Bool) =
    // combinational => data valid on oready
    new RegWriteFn(true, { case (_, oready, data) =>
      (true.B, x(oready, data))
    })

  // write to a DecoupledIO (only safe if there is a consistent sink draining data)
  // NOTE: this is not an IrrevocableIO (even on TL2) because other fields could cause a lowered valid
  implicit def apply(x: DecoupledIO[UInt]): RegWriteFn = RegWriteFn((valid, data) => {
    x.valid := valid;
    x.bits := data;
    x.ready
  })

  // updates a register (or adds a mux to a wire)
  implicit def apply(x: UInt): RegWriteFn = RegWriteFn((valid, data) => {
    when(valid) {
      x := data
    };
    true.B
  })

  // noop
  implicit def apply(x: Unit): RegWriteFn = RegWriteFn((valid, data) => {
    true.B
  })
}

case class RegField(width: Int, read: RegReadFn, write: RegWriteFn, desc: Option[RegFieldDesc]) {
  require(width >= 0, s"RegField width must be >= 0, not $width")

  def pipelined = !read.combinational || !write.combinational

  def readOnly = this.copy(write = (), desc = this.desc.map(_.copy(access = RegFieldAccessType.R)))

  /* TODO: expose these API to OM?
  def toJson(byteOffset: Int, bitOffset: Int): JValue = {
    (("byteOffset" -> s"0x${byteOffset.toHexString}") ~
      ("bitOffset" -> bitOffset) ~
      ("bitWidth" -> width) ~
      ("name" -> desc.map(_.name)) ~
      ("description" -> desc.map { d => if (d.desc == "") None else Some(d.desc) }) ~
      ("resetValue" -> desc.map {
        _.reset
      }) ~
      ("group" -> desc.map {
        _.group
      }) ~
      ("groupDesc" -> desc.map {
        _.groupDesc
      }) ~
      ("accessType" -> desc.map { d => d.access.toString }) ~
      ("writeType" -> desc.map { d => d.wrType.map(_.toString) }) ~
      ("readAction" -> desc.map { d => d.rdAction.map(_.toString) }) ~
      ("volatile" -> desc.map { d => if (d.volatile) Some(true) else None }) ~
      ("enumerations" -> desc.map { d =>
        Option(d.enumerations.map { case (key, (name, edesc)) =>
          (("value" -> key) ~ ("name" -> name) ~ ("description" -> edesc))
        }).filter(_.nonEmpty)
      }))
  }
  */
}

object RegField {
  // Byte address => sequence of bitfields, lowest index => the lowest address
  type Map = (Int, Seq[RegField])

  def apply(n: Int): RegField = apply(n, (), (), Some(RegFieldDesc.reserved))

  def apply(n: Int, desc: RegFieldDesc): RegField = apply(n, (), (), Some(desc))

  def apply(n: Int, r: RegReadFn, w: RegWriteFn): RegField = apply(n, r, w, None)

  def apply(n: Int, r: RegReadFn, w: RegWriteFn, desc: RegFieldDesc): RegField = apply(n, r, w, Some(desc))

  def apply(n: Int, rw: UInt): RegField = apply(n, rw, rw, None)

  def apply(n: Int, rw: UInt, desc: RegFieldDesc): RegField = apply(n, rw, rw, Some(desc))

  def r(n: Int, r: RegReadFn): RegField = apply(n, r, (), None)

  def r(n: Int, r: RegReadFn, desc: RegFieldDesc): RegField = apply(n, r, (), Some(desc.copy(access = RegFieldAccessType.R)))

  def w(n: Int, w: RegWriteFn): RegField = apply(n, (), w, None)

  def w(n: Int, w: RegWriteFn, desc: RegFieldDesc): RegField = apply(n, (), w, Some(desc.copy(access = RegFieldAccessType.W)))

  // This RegField allows 'set' to set bits in 'reg'.
  // and to clear bits when the bus writes bits of value 1.
  // Setting takes priority over clearing.
  def w1ToClear(n: Int, reg: UInt, set: UInt, desc: Option[RegFieldDesc] = None): RegField =
    RegField(n, reg, RegWriteFn((valid, data) => {
      reg := (~((~reg) | Mux(valid, data, 0.U))) | set;
      true.B
    }),
      desc.map {
        _.copy(access = RegFieldAccessType.RW, wrType = Some(RegFieldWrType.ONE_TO_CLEAR), volatile = true)
      })

  // This RegField wraps an explicit register
  // (e.g. Black-Boxed Register) to create a R/W register.
  def rwReg(n: Int, bb: SimpleRegIO, desc: Option[RegFieldDesc] = None): RegField =
    RegField(n, bb.q, RegWriteFn((valid, data) => {
      bb.en := valid
      bb.d := data
      true.B
    }), desc)

  // Create byte-sized read-write RegFields out of a large UInt register.
  // It is updated when any of the (implemented) bytes are written, the non-written
  // bytes are just copied over from their current value.
  // Because the RegField are all byte-sized, this is also suitable when a register is larger
  // than the intended bus width of the device (atomic updates are impossible).
  def bytes(reg: UInt, numBytes: Int, desc: Option[RegFieldDesc]): Seq[RegField] = {
    require(reg.getWidth * 8 >= numBytes, "Can't break a ${reg.getWidth}-bit-wide register into only ${numBytes} bytes.")
    val numFullBytes = reg.getWidth / 8
    val numPartialBytes = if ((reg.getWidth % 8) > 0) 1 else 0
    val numPadBytes = numBytes - numFullBytes - numPartialBytes
    val pad = reg | 0.U((8 * numBytes).W)
    val oldBytes = VecInit.tabulate(numBytes) { i => pad(8 * (i + 1) - 1, 8 * i) }
    val newBytes = WireDefault(oldBytes)
    val valids = WireDefault(VecInit.fill(numBytes) {
      false.B
    })
    when(valids.reduce(_ || _)) {
      reg := newBytes.asUInt
    }

    def wrFn(i: Int): RegWriteFn = RegWriteFn((valid, data) => {
      valids(i) := valid
      when(valid) {
        newBytes(i) := data
      }
      true.B
    })

    val fullBytes = Seq.tabulate(numFullBytes) { i =>
      val newDesc = desc.map { d => d.copy(name = d.name + s"_$i") }
      RegField(8, oldBytes(i), wrFn(i), newDesc)
    }
    val partialBytes = if (numPartialBytes > 0) {
      val newDesc = desc.map { d => d.copy(name = d.name + s"_$numFullBytes") }
      Seq(RegField(reg.getWidth % 8, oldBytes(numFullBytes), wrFn(numFullBytes), newDesc),
        RegField(8 - (reg.getWidth % 8)))
    } else Nil
    val padBytes = Seq.fill(numPadBytes) {
      RegField(8)
    }
    fullBytes ++ partialBytes ++ padBytes
  }

  def bytes(reg: UInt, desc: Option[RegFieldDesc]): Seq[RegField] = {
    val width = reg.getWidth
    require(width % 8 == 0, s"RegField.bytes must be called on byte-sized reg, not ${width} bits")
    bytes(reg, width / 8, desc)
  }

  def bytes(reg: UInt, numBytes: Int): Seq[RegField] = bytes(reg, numBytes, None)

  def bytes(reg: UInt): Seq[RegField] = bytes(reg, None)
}