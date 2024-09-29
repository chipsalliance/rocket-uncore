// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2016-2017 SiFive, Inc
// SPDX-FileCopyrightText: 2024 Jiuyang Liu <liu@jiuyang.me>

package org.chipsalliance.uncore.plic

import chisel3._
import chisel3.experimental.dataview.DataViewable
import chisel3.experimental.hierarchy.{instantiable, Instance, Instantiate}
import chisel3.experimental.{SerializableModule, SerializableModuleParameter}
import chisel3.probe.{define, Probe, ProbeValue}
import chisel3.properties.{AnyClassType, Class, Property}
import chisel3.util.{log2Ceil, log2Up, Cat, PopCount, UIntToOH}
import org.chipsalliance.amba.RegMapper.regmap
import org.chipsalliance.amba._
import org.chipsalliance.amba.axi4.bundle._

object PLICParameter {
  implicit def rwP: upickle.default.ReadWriter[PLICParameter] =
    upickle.default.macroRW
}

/** Parameter of [[PLIC]] */
case class PLICParameter(
  useAsyncReset: Boolean,
  base:          BigInt,
  contexts:      Int,
  nDevices:      Int,
  maxPriorities: Int,
  axi4parameter: AXI4BundleParameter)
    extends SerializableModuleParameter {
  def minPriorities = math.min(maxPriorities, nDevices)

  def nPriorities = (1 << log2Ceil(minPriorities + 1)) - 1 // round up to next 2^n-1

  val prioBits = log2Ceil(nPriorities + 1)

  val maxDevices = 1023
  val maxContexts = 15872
  val priorityBase = 0x0
  val pendingBase = 0x1000
  val enableBase = 0x2000
  val hartBase = 0x200000

  def claimOffset = 4
  def priorityBytes = 4

  def enableOffset(i: Int) = i * ((maxDevices + 7) / 8)
  def enableBase(i:   Int): Int = enableOffset(i) + enableBase

  def hartOffset(i: Int) = i * 0x1000
  def hartBase(i:   Int): Int = hartOffset(i) + hartBase

  val firstEnable = nDevices.min(7)
  val fullEnables = (nDevices - firstEnable) / 8
  val tailEnable = nDevices - firstEnable - 8 * fullEnables

  require(nDevices <= maxDevices, s"Must be: nDevices=$nDevices <= .maxDevices=$maxDevices")
  require(
    contexts > 0 && contexts <= maxContexts,
    s"Must be: contexts=$contexts > 0 && contexts <= maxContexts=$maxContexts"
  )
}

/** Verification IO of [[PLIC]] */
class PLICProbe(parameter: PLICParameter) extends Bundle {}

/** Metadata of [[PLIC]]. */
@instantiable
class PLICOM(parameter: PLICParameter) extends Class {
  val useAsyncReset: Property[Boolean] = IO(Output(Property[Boolean]()))
  useAsyncReset := Property(parameter.useAsyncReset)
}

/** Interface of [[PLIC]]. */
class PLICInterface(parameter: PLICParameter) extends Bundle {
  val clock = Input(Clock())
  val reset = Input(if (parameter.useAsyncReset) AsyncReset() else Bool())
  val probe = Output(Probe(new PLICProbe(parameter), layers.Verification))
  val om = Output(Property[AnyClassType]())

  val plic = Flipped(axi4.bundle.verilog.irrevocable(parameter.axi4parameter)).asInstanceOf[AXI4RWIrrevocableVerilog]
  val source = Input(Vec(parameter.nDevices, Bool()))
  val ip = Output(Vec(parameter.contexts, Bool()))
}

/** Hardware Implementation of PLIC [https://github.com/riscv/riscv-plic-spec/blob/master/riscv-plic.adoc] */
@instantiable
class PLIC(val parameter: PLICParameter)
    extends FixedIORawModule(new PLICInterface(parameter))
    with SerializableModule[PLICParameter]
    with ImplicitClock
    with ImplicitReset {
  override protected def implicitClock: Clock = io.clock

  override protected def implicitReset: Reset = io.reset

  // Assign Probe
  val probeWire: PLICProbe = Wire(new PLICProbe(parameter))
  define(io.probe, ProbeValue(probeWire))

  // Assign Metadata
  val omInstance: Instance[PLICOM] = Instantiate(new PLICOM(parameter))
  io.om := omInstance.getPropertyReference.asAnyClassType

  val priority =
    if (parameter.nPriorities > 0)
      Seq.tabulate(parameter.nDevices)(idx => Reg(UInt(parameter.prioBits.W)).suggestName(s"priority_${idx}"))
    else Seq.tabulate(parameter.nDevices.max(1))(idx => WireDefault(1.U))
  val threshold =
    if (parameter.nPriorities > 0)
      Seq.tabulate(parameter.contexts)(idx => Reg(UInt(parameter.prioBits.W)).suggestName(s"threshold_${idx}"))
    else Seq.tabulate(parameter.contexts)(idx => Reg(0.U))
  val pending = Seq.tabulate(parameter.nDevices.max(1))(idx => Reg(false.B).suggestName(s"pending_${idx}"))

  /* Construct the enable registers, chunked into 8-bit segments to reduce verilog size */
  def enableRegs = (Reg(UInt(parameter.firstEnable.W)) +:
    Seq.fill(parameter.fullEnables) { Reg(UInt(8.W)) }) ++
    (if (parameter.tailEnable > 0) Some(Reg(UInt(parameter.tailEnable.W))) else None)
  val enables = Seq.fill(parameter.contexts) { enableRegs }
  val enableVec = VecInit(enables.map(x => Cat(x.reverse)))
  val enableVec0 = VecInit(enableVec.map(x => Cat(x, 0.U(1.W))))

  val maxDevs = Seq.tabulate(parameter.contexts)(idx => Reg(UInt(log2Ceil(parameter.nDevices + 1).W)))
  val pendingUInt = Cat(pending.reverse)

  if (parameter.nDevices > 0) {
    def findMax(x: Seq[UInt]): (UInt, UInt) = {
      object MuxT {
        def apply[T <: Data, U <: Data](cond: Bool, con: (T, U), alt: (T, U)): (T, U) =
          (Mux(cond, con._1, alt._1), Mux(cond, con._2, alt._2))

        def apply[T <: Data, U <: Data, W <: Data](cond: Bool, con: (T, U, W), alt: (T, U, W)): (T, U, W) =
          (Mux(cond, con._1, alt._1), Mux(cond, con._2, alt._2), Mux(cond, con._3, alt._3))

        def apply[T <: Data, U <: Data, W <: Data, X <: Data](cond: Bool, con: (T, U, W, X), alt: (T, U, W, X))
          : (T, U, W, X) =
          (Mux(cond, con._1, alt._1), Mux(cond, con._2, alt._2), Mux(cond, con._3, alt._3), Mux(cond, con._4, alt._4))
      }
      if (x.length > 1) {
        val half = 1 << (log2Ceil(x.length) - 1)
        val left = findMax(x.take(half))
        val right = findMax(x.drop(half))
        MuxT(left._1 >= right._1, left, (right._1, half.U | right._2))
      } else (x.head, 0.U)
    }
    Seq.tabulate(parameter.contexts) { hart =>
      val ip = enableVec(hart) & pendingUInt
      val effectivePriority = (1.U << parameter.prioBits) +: (ip.asBools.zip(priority)).map { case (p, x) => Cat(p, x) }
      val (maxPri, maxDev) = findMax(effectivePriority)

      maxDevs(hart) := maxDev // strips the always-constant high '1' bit
      io.ip(hart) := RegNext(maxPri) > threshold(hart)
    }
  }

  val priorityRegFields = {
    def priorityRegField(x: UInt, i: Int) = {
      def priorityRegDesc(i: Int) =
        RegFieldDesc(
          name = s"priority_$i",
          desc = s"acting priority of interrupt source $i",
          group = Some(s"priority_${i}"),
          groupDesc = Some(s"acting priority of interrupt source $i"),
          reset = if (parameter.nPriorities > 0) None else Some(1)
        )

      if (parameter.nPriorities > 0) {
        RegField(parameter.prioBits, x, priorityRegDesc(i))
      } else {
        RegField.r(parameter.prioBits, x, priorityRegDesc(i))
      }
    }
    priority.zipWithIndex.map { case (p, i) =>
      parameter.priorityBase + parameter.priorityBytes * (i + 1) ->
        Seq(priorityRegField(p, i + 1))
    }
  }
  val pendingRegFields = {
    def pendingRegDesc(i: Int) =
      RegFieldDesc(
        name = s"pending_$i",
        desc = s"Set to 1 if interrupt source $i is pending, regardless of its enable or priority setting.",
        group = Some("pending"),
        groupDesc = Some("Pending Bit Array. 1 Bit for each interrupt source."),
        volatile = true
      )

    Seq(
      parameter.pendingBase ->
        (RegField(1) +: pending.zipWithIndex.map { case (b, i) => RegField.r(1, b, pendingRegDesc(i + 1)) })
    )
  }
  val enableRegFields = {
    def enableRegDesc(i: Int, j: Int, wide: Int) = {
      val low = if (j == 0) 1 else j * 8
      val high = low + wide - 1
      RegFieldDesc(
        name = s"enables_${j}",
        desc = s"Targets ${low}-${high}. Set bits to 1 if interrupt should be enabled.",
        group = Some(s"enables_${i}"),
        groupDesc = Some(s"Enable bits for each interrupt source for target $i. 1 bit for each interrupt source.")
      )
    }

    enables.zipWithIndex.map { case (e, i) =>
      parameter.enableBase(i) -> (RegField(1) +: e.zipWithIndex.map { case (x, j) =>
        RegField(x.getWidth, x, enableRegDesc(i, j, x.getWidth))
      })
    }
  }

  // When a hart writes a claim/complete register, then
  // the written device (as long as it is actually enabled for that
  // hart) is marked complete.
  // This code exploits the fact that, practically, only one claim/complete register
  // can be written at a time. We check for this because if the address map
  // were to change, it may no longer be true.
  // Note -- PLIC doesn't care which hart writes the register.
  val claimer = Wire(Vec(parameter.contexts, Bool()))
  assert(PopCount(claimer.asUInt) <= 1.U) // One-Hot
  val claiming = Seq.tabulate(parameter.contexts) { i => Mux(claimer(i), maxDevs(i), 0.U) }.reduceLeft(_ | _)
  val claimedDevs = VecInit(UIntToOH(claiming, parameter.nDevices + 1).asBools)
  (pending.zip(claimedDevs.tail)).foreach { case (p, c) =>
    when(c) { p := !c }
  }

  // When a hart writes a claim/complete register, then
  // the written device (as long as it is actually enabled for that
  // hart) is marked complete.
  // This code exploits the fact that, practically, only one claim/complete register
  // can be written at a time. We check for this because if the address map
  // were to change, it may no longer be true.
  // Note -- PLIC doesn't care which hart writes the register.
  val completer = Wire(Vec(parameter.contexts, Bool()))
  assert(PopCount(completer.asUInt) <= 1.U) // One-Hot
  val completerDev = Wire(UInt(log2Up(parameter.nDevices + 1).W))
  val completedDevs = Mux(completer.reduce(_ || _), UIntToOH(completerDev, parameter.nDevices + 1), 0.U)

  val hartRegFields = {
    def thresholdRegField(x: UInt, i: Int) = {
      def thresholdRegDesc(i: Int) =
        RegFieldDesc(
          name = s"threshold_$i",
          desc = s"Interrupt & claim threshold for target $i. Maximum value is ${parameter.nPriorities}.",
          reset = if (parameter.nPriorities > 0) None else Some(1)
        )

      if (parameter.nPriorities > 0) {
        RegField(parameter.prioBits, x, thresholdRegDesc(i))
      } else {
        RegField.r(parameter.prioBits, x, thresholdRegDesc(i))
      }
    }
    Seq.tabulate(parameter.contexts) { i =>
      parameter.hartBase(i) -> Seq(
        thresholdRegField(threshold(i), i),
        RegField(32 - parameter.prioBits),
        RegField(
          32,
          RegReadFn { valid =>
            claimer(i) := valid
            (true.B, maxDevs(i))
          },
          RegWriteFn { (valid, data) =>
            assert(
              completerDev === data(log2Ceil(parameter.nDevices + 1) - 1, 0),
              "completerDev should be consistent for all harts"
            )
            completerDev := data(log2Ceil(parameter.nDevices + 1) - 1, 0)
            completer(i) := valid && enableVec0(i)(completerDev)
            true.B
          },
          Some(
            RegFieldDesc(
              s"claim_complete_$i",
              s"Claim/Complete register for Target $i. Reading this register returns the claimed interrupt number and makes it no longer pending." +
                s"Writing the interrupt number back completes the interrupt.",
              reset = None,
              wrType = Some(RegFieldWrType.MODIFY),
              rdAction = Some(RegFieldRdAction.MODIFY),
              volatile = true
            )
          )
        )
      )
    }
  }

  regmap(
    io.plic.viewAs[AXI4RWIrrevocable],
    0,
    false,
    (priorityRegFields ++ pendingRegFields ++ enableRegFields ++ hartRegFields): _*
  )
}
