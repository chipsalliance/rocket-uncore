// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2016-2017 SiFive, Inc.
// SPDX-FileCopyrightText: 2024 Jiuyang Liu <liu@jiuyang.me>

// After finishing one or two riscv uncore ip developing, it will be upstreamed to chipsalliance/amba
package org.chipsalliance.amba

import chisel3._
import chisel3.experimental.SourceInfo
import chisel3.util._

import scala.reflect.ClassTag

case class RegMapperParams(indexBits: Int, maskBits: Int, idBits: Int)

class RegMapperInput(val params: RegMapperParams) extends Bundle {
  val read = Bool()
  val index = UInt(params.indexBits.W)
  val data = UInt((params.maskBits * 8).W)
  val mask = UInt(params.maskBits.W)
  val id = UInt(params.idBits.W)
}

class RegMapperOutput(val params: RegMapperParams) extends Bundle {
  val read = Bool()
  val data = UInt((params.maskBits * 8).W)
  val id = UInt(params.idBits.W)
}

object RegMapper {

  // Calling this method causes the matching AXI4 bundle to be
  // configured to route all requests to the listed RegFields.
  def regmap(io: org.chipsalliance.amba.axi4.bundle.AXI4RWIrrevocable, concurrency: Int, undefZero: Boolean, mapping: RegField.Map*) = {
    val beatBytes: Int = io.dataWidth / 8
    val addrMask: Int = io.dataWidth / 8
    val idBits: Int = io.idWidth

    val ar = io.ar
    val aw = io.aw
    val w = io.w
    val r = io.r
    val b = io.b

    val params = RegMapperParams(log2Up((addrMask + 1) / beatBytes), beatBytes, idBits)
    val in = Wire(Decoupled(new RegMapperInput(params)))

    // Prefer to execute reads first
    in.valid := ar.valid || (aw.valid && w.valid)
    ar.ready := in.ready
    aw.ready := in.ready && !ar.valid && w.valid
    w.ready := in.ready && !ar.valid && aw.valid

    val addr = Mux(ar.valid, ar.bits.addr, aw.bits.addr)
    val mask = MaskGen(ar.bits.addr, ar.bits.size, beatBytes)

    in.bits.read := ar.valid
    in.bits.index := addr >> log2Ceil(beatBytes)
    in.bits.data := w.bits.data
    in.bits.mask := Mux(ar.valid, mask, w.bits.strb)
    in.bits.id := Mux(ar.valid, ar.bits.id, aw.bits.id)

    // Invoke the register map builder and make it Irrevocable
    val out = Queue.irrevocable(
      RegMapper(beatBytes, concurrency, undefZero, in, mapping: _*),
      entries = 2)

    // No flow control needed
    out.ready := Mux(out.bits.read, r.ready, b.ready)
    r.valid := out.valid && out.bits.read
    b.valid := out.valid && !out.bits.read

    r.bits.id := out.bits.id
    r.bits.data := out.bits.data
    r.bits.last := true.B
    r.bits.resp := 0.U

    b.bits.id := out.bits.id
    b.bits.resp := 0.U
  }

  // Create a generic register-based device
  def apply(bytes: Int, concurrency: Int, undefZero: Boolean, in: DecoupledIO[RegMapperInput], mapping: RegField.Map*)(implicit sourceInfo: SourceInfo) = {
    // Filter out zero-width fields
    val bytemap: Seq[(Int, Seq[RegField])] = mapping.toList.map { case (offset, fields) => (offset, fields.filter(_.width != 0)) }

    // Negative addresses are bad
    bytemap.foreach { byte => require(byte._1 >= 0) }

    // Transform all fields into bit offsets Seq[(bit, field)]
    val bitmap: Seq[(Int, RegField)] = bytemap.flatMap { case (byte, fields) =>
      val bits = fields.scanLeft(byte * 8)(_ + _.width).init
      bits zip fields
    }.sortBy(_._1)

    // Detect overlaps
    (bitmap.init zip bitmap.tail) foreach { case ((lbit, lfield), (rbit, rfield)) =>
      require(lbit + lfield.width <= rbit, s"Register map overlaps at bit ${rbit}.")
    }

    // Group those fields into bus words Map[word, List[(bit, field)]]
    val wordmap: Map[Int, Seq[(Int, RegField)]] = bitmap.groupBy(_._1 / (8 * bytes))

    // Make sure registers fit
    val inParams = in.bits.params
    val inBits = inParams.indexBits
    assert(wordmap.keySet.max < (1 << inBits), "Register map does not fit in device")

    val out = Wire(Decoupled(new RegMapperOutput(inParams)))
    val front = Wire(Decoupled(new RegMapperInput(inParams)))
    front.bits := in.bits

    // Must this device pipeline the control channel?
    val pipelined = wordmap.values.map(_.map(_._2.pipelined)).flatten.reduce(_ || _)
    val depth = concurrency
    require(depth >= 0)
    require(!pipelined || depth > 0, "Register-based device with request/response handshaking needs concurrency > 0")
    val back = if (depth > 0) {
      val front_q = Module(new Queue(new RegMapperInput(inParams), depth) {
        override def desiredName = s"Queue${depth}_${front.bits.typeName}_i${inParams.indexBits}_m${inParams.maskBits}"
      })
      front_q.io.enq <> front
      front_q.io.deq
    } else front

    // Convert to and from Bits
    def toBits(x: Int, tail: List[Boolean] = List.empty): List[Boolean] =
      if (x == 0) tail.reverse else toBits(x >> 1, ((x & 1) == 1) :: tail)

    def ofBits(bits: List[Boolean]) = bits.foldRight(0) { case (x, y) => (if (x) 1 else 0) | y << 1 }

    // Find the minimal mask that can decide the register map
    val mask = AddressDecoder(wordmap.keySet.toList)
    val maskMatch = ~mask.U(inBits.W)
    val maskFilter = toBits(mask)
    val maskBits = maskFilter.count(x => x)

    // Calculate size and indexes into the register map
    val regSize = 1 << maskBits

    def regIndexI(x: Int) = ofBits((maskFilter zip toBits(x)).filter(_._1).map(_._2))

    def regIndexU(x: UInt) = if (maskBits == 0) 0.U else
      Cat((maskFilter zip x.asBools).filter(_._1).map(_._2).reverse)

    val findex = front.bits.index & maskMatch
    val bindex = back.bits.index & maskMatch

    // Protection flag for undefined registers
    val iRightReg = Array.fill(regSize) {
      true.B
    }
    val oRightReg = Array.fill(regSize) {
      true.B
    }

    // Transform the wordmap into minimal decoded indexes, Seq[(index, bit, field)]
    val flat: Seq[(Int, Int, RegField)] = wordmap.toList.flatMap { case (word, fields) =>
      val index = regIndexI(word)
      if (undefZero) {
        val uint = (word & ~mask).U(inBits.W)
        iRightReg(index) = findex === uint
        oRightReg(index) = bindex === uint
      }
      // Confirm that no field spans a word boundary
      fields foreach { case (bit, field) =>
        val off = bit - 8 * bytes * word
        // println(s"Reg ${word}: [${off}, ${off+field.width})")
        require(off + field.width <= bytes * 8, s"Field at word ${word}*(${bytes}B) has bits [${off}, ${off + field.width}), which exceeds word limit.")
      }
      // println("mapping 0x%x -> 0x%x for 0x%x/%d".format(word, index, mask, maskBits))
      fields.map { case (bit, field) => (index, bit - 8 * bytes * word, field) }
    }
    // Forward declaration of all flow control signals
    val rivalid = Wire(Vec(flat.size, Bool()))
    val wivalid = Wire(Vec(flat.size, Bool()))
    val roready = Wire(Vec(flat.size, Bool()))
    val woready = Wire(Vec(flat.size, Bool()))

    // Per-register list of all control signals needed for data to flow
    val rifire = Array.fill(regSize) {
      Nil: List[(Bool, Bool)]
    }
    val wifire = Array.fill(regSize) {
      Nil: List[(Bool, Bool)]
    }
    val rofire = Array.fill(regSize) {
      Nil: List[(Bool, Bool)]
    }
    val wofire = Array.fill(regSize) {
      Nil: List[(Bool, Bool)]
    }

    // The output values for each register
    val dataOut = Array.fill(regSize) {
      0.U
    }

    // Which bits are touched?
    val frontMask = FillInterleaved(8, front.bits.mask)
    val backMask = FillInterleaved(8, back.bits.mask)

    // Connect the fields
    for (i <- 0 until flat.size) {
      val (reg, low, field) = flat(i)
      val high = low + field.width - 1
      // Confirm that no register is too big
      require(high < 8 * bytes)
      val rimask = frontMask(high, low).orR
      val wimask = frontMask(high, low).andR
      val romask = backMask(high, low).orR
      val womask = backMask(high, low).andR
      val data = if (field.write.combinational) back.bits.data else front.bits.data
      val f_rivalid = rivalid(i) && rimask
      val f_roready = roready(i) && romask
      val f_wivalid = wivalid(i) && wimask
      val f_woready = woready(i) && womask
      val (f_riready, f_rovalid, f_data) = field.read.fn(f_rivalid, f_roready)
      val (f_wiready, f_wovalid) = field.write.fn(f_wivalid, f_woready, data(high, low))

      // cover reads and writes to register
      val fname = field.desc.map {
        _.name
      }.getOrElse("")
      val fdesc = field.desc.map {
        _.desc + ":"
      }.getOrElse("")

      val facct = field.desc.map {
        _.access
      }.getOrElse("")
      //      if((facct == RegFieldAccessType.R) || (facct == RegFieldAccessType.RW)) {
      //        property.cover(f_rivalid && f_riready, fname + "_Reg_read_start",  fdesc + " RegField Read Request Initiate")
      //        property.cover(f_rovalid && f_roready, fname + "_Reg_read_out",    fdesc + " RegField Read Request Complete")
      //      }
      //      if((facct == RegFieldAccessType.W) || (facct == RegFieldAccessType.RW)) {
      //        property.cover(f_wivalid && f_wiready, fname + "_Reg_write_start", fdesc + " RegField Write Request Initiate")
      //        property.cover(f_wovalid && f_woready, fname + "_Reg_write_out",   fdesc + " RegField Write Request Complete")
      //      }

      def litOR(x: Bool, y: Bool) = if (x.isLit && x.litValue == 1) true.B else x || y
      // Add this field to the ready-valid signals for the register
      rifire(reg) = (rivalid(i), litOR(f_riready, !rimask)) +: rifire(reg)
      wifire(reg) = (wivalid(i), litOR(f_wiready, !wimask)) +: wifire(reg)
      rofire(reg) = (roready(i), litOR(f_rovalid, !romask)) +: rofire(reg)
      wofire(reg) = (woready(i), litOR(f_wovalid, !womask)) +: wofire(reg)

      // ... this loop iterates from smallest to largest bit offset
      val prepend = if (low == 0) {
        f_data
      } else {
        Cat(f_data, dataOut(reg) | 0.U(low.W))
      }
      dataOut(reg) = (prepend | 0.U((high + 1).W))(high, 0)
    }

    // Which register is touched?
    val iindex = regIndexU(front.bits.index)
    val oindex = regIndexU(back.bits.index)
    val frontSel = UIntToOH(iindex).asBools
    val backSel = UIntToOH(oindex).asBools

    // Compute: is the selected register ready? ... and cross-connect all ready-valids
    def mux(index: UInt, valid: Bool, select: Seq[Bool], guard: Seq[Bool], flow: Seq[Seq[(Bool, Bool)]]): Bool =
      MuxSeq(index, true.B, ((select zip guard) zip flow).map { case ((s, g), f) =>
        val out = Wire(Bool())
        ReduceOthers((out, valid && s && g) +: f)
        out || !g
      })

    // Include the per-register one-hot selected criteria
    val rifireMux = mux(iindex, in.valid && front.ready && front.bits.read, frontSel, iRightReg, rifire)
    val wifireMux = mux(iindex, in.valid && front.ready && !front.bits.read, frontSel, iRightReg, wifire)
    val rofireMux = mux(oindex, back.valid && out.ready && back.bits.read, backSel, oRightReg, rofire)
    val wofireMux = mux(oindex, back.valid && out.ready && !back.bits.read, backSel, oRightReg, wofire)

    val iready = Mux(front.bits.read, rifireMux, wifireMux)
    val oready = Mux(back.bits.read, rofireMux, wofireMux)

    // Connect the pipeline
    in.ready := front.ready && iready
    front.valid := in.valid && iready
    back.ready := out.ready && oready
    out.valid := back.valid && oready

    out.bits.read := back.bits.read
    out.bits.data := Mux(MuxSeq(oindex, true.B, oRightReg),
      MuxSeq(oindex, 0.U, dataOut),
      0.U)
    out.bits.id := back.bits.id

    out
  }
}


object MuxSeq {
  def apply[T <: Data : ClassTag](index: UInt, default: T, first: T, rest: T*): T =
    apply(index, default, first :: rest.toList)

  def apply[T <: Data : ClassTag](index: UInt, default: T, cases: Seq[T]): T =
    MuxTable(index, default, cases.zipWithIndex.map { case (v, i) => (BigInt(i), v) })
}

object MuxTable {
  def apply[T <: Data : ClassTag](index: UInt, default: T, first: (BigInt, T), rest: (BigInt, T)*): T =
    apply(index, default, first :: rest.toList)

  def apply[T <: Data : ClassTag](index: UInt, default: T, cases: Seq[(BigInt, T)]): T = {
    /* All keys must be >= 0 and distinct */
    cases.foreach { case (k, _) => require(k >= 0) }
    require(cases.map(_._1).distinct.size == cases.size)

    /* Filter out any cases identical to the default */
    val simple = cases.filter { case (k, v) => !default.isLit || !v.isLit || v.litValue != default.litValue }

    val maxKey = (BigInt(0) +: simple.map(_._1)).max
    val endIndex = BigInt(1) << log2Ceil(maxKey + 1)

    if (simple.isEmpty) {
      default
    } else if (endIndex <= 2 * simple.size) {
      /* The dense encoding case uses a Vec */
      val table = Array.fill(endIndex.toInt) {
        default
      }
      simple.foreach { case (k, v) => table(k.toInt) = v }
      Mux(index >= endIndex.U, default, VecInit(table)(index))
    } else {
      /* The sparse encoding case uses switch */
      val out = WireDefault(default)
      simple.foldLeft(new chisel3.util.SwitchContext(index, None, Set.empty)) { case (acc, (k, v)) =>
        acc.is(k.U) {
          out := v
        }
      }
      out
    }
  }
}

object ReduceOthers {
  // Given a list of bools, create this output:
  //   out[i] = AND[j=0..out.size, j!=i] in[j]
  def apply(x: Seq[Bool]): Seq[Bool] = {
    val (literals, variables) = x.partition(_.isLit)

    val falses = literals.count(_.litValue == 0)
    if (falses > 2) {
      Seq.fill(x.size) {
        false.B
      }
    } else if (falses == 1) {
      x.map { b =>
        if (b.isLit && b.litValue == 0) {
          variables.foldLeft(true.B)(_ && _)
        } else {
          false.B
        }
      }
    } else {
      var (out, all) = helper(variables)
      x.map { b =>
        if (b.isLit) {
          all
        } else {
          val sel = out.head
          out = out.tail
          sel
        }
      }
    }
  }

  // Take pairs of (output_wire, input_bool)
  def apply(x: Seq[(Bool, Bool)]): Unit = {
    (x.map(_._1) zip apply(x.map(_._2))) foreach { case (w, x) => w := x }
  }

  private def helper(x: Seq[Bool]): (Seq[Bool], Bool) = {
    if (x.size <= 1) {
      (Seq.fill(x.size) {
        true.B
      }, x.headOption.getOrElse(true.B))
    } else if (x.size <= 3) {
      (Seq.tabulate(x.size) { i =>
        (x.take(i) ++ x.drop(i + 1)).reduce(_ && _)
      }, x.reduce(_ && _))
    } else {
      val (half, all) = helper(x.grouped(2).map(_.reduce(_ && _)).toList)
      (Seq.tabulate(x.size) { i =>
        if ((i ^ 1) >= x.size) half(i / 2) else x(i ^ 1) && half(i / 2)
      }, all)
    }
  }
}

case class AddressSet(base: BigInt, mask: BigInt) extends Ordered[AddressSet] {
  def overlaps(x: AddressSet) = (~(mask | x.mask) & (base ^ x.base)) == 0

  def widen(imask: BigInt) = AddressSet(base & ~imask, mask | imask)

  // AddressSets have one natural Ordering (the containment order, if contiguous)
  def compare(x: AddressSet) = {
    val primary = (this.base - x.base).signum // smallest address first
    val secondary = (x.mask - this.mask).signum // largest mask first
    if (primary != 0) primary else secondary
  }
}

object AddressDecoder {
  type Port = Seq[AddressSet]
  type Ports = Seq[Port]
  type Partition = Ports
  type Partitions = Seq[Partition]

  val addressOrder = Ordering.ordered[AddressSet]
  val portOrder = Ordering.Iterable(addressOrder)
  val partitionOrder = Ordering.Iterable(portOrder)

  // Find the minimum subset of bits needed to disambiguate port addresses.
  // ie: inspecting only the bits in the output, you can look at an address
  //     and decide to which port (outer Seq) the address belongs.
  def apply(ports: Ports, givenBits: BigInt = BigInt(0)): BigInt = {
    val nonEmptyPorts = ports.filter(_.nonEmpty)
    if (nonEmptyPorts.size <= 1) {
      givenBits
    } else {
      // Verify the user did not give us an impossible problem
      nonEmptyPorts.combinations(2).foreach { case Seq(x, y) =>
        x.foreach { a =>
          y.foreach { b =>
            require(!a.overlaps(b), s"Ports cannot overlap: $a $b")
          }
        }
      }

      val maxBits = log2Ceil(1 + nonEmptyPorts.map(_.map(_.base).max).max)
      val (bitsToTry, bitsToTake) = (0 until maxBits).map(BigInt(1) << _).partition(b => (givenBits & b) == 0)
      val partitions = Seq(nonEmptyPorts.map(_.sorted).sorted(portOrder))
      val givenPartitions = bitsToTake.foldLeft(partitions) { (p, b) => partitionPartitions(p, b) }
      val selected = recurse(givenPartitions, bitsToTry.reverse.toSeq)
      val output = selected.reduceLeft(_ | _) | givenBits

      // Modify the AddressSets to allow the new wider match functions
      val widePorts = nonEmptyPorts.map {
        _.map {
          _.widen(~output)
        }
      }
      // Verify that it remains possible to disambiguate all ports
      widePorts.combinations(2).foreach { case Seq(x, y) =>
        x.foreach { a =>
          y.foreach { b =>
            require(!a.overlaps(b), s"Ports cannot overlap: $a $b")
          }
        }
      }

      output
    }
  }

  // A simpler version that works for a Seq[Int]
  def apply(keys: Seq[Int]): Int = {
    val ports = keys.map(b => Seq(AddressSet(b, 0)))
    apply(ports).toInt
  }

  // The algorithm has a set of partitions, discriminated by the selected bits.
  // Each partion has a set of ports, listing all addresses that lead to that port.
  // Seq[Seq[Seq[AddressSet]]]
  //         ^^^^^^^^^^^^^^^ set of addresses that are routed out this port
  //     ^^^ the list of ports
  // ^^^ cases already distinguished by the selected bits thus far
  //
  // Solving this problem is NP-hard, so we use a simple greedy heuristic:
  //   pick the bit which minimizes the number of ports in each partition
  //   as a secondary goal, reduce the number of AddressSets within a partition

  def bitScore(partitions: Partitions): Seq[Int] = {
    val maxPortsPerPartition = partitions.map(_.size).max
    val maxSetsPerPartition = partitions.map(_.map(_.size).sum).max
    val sumSquarePortsPerPartition = partitions.map(p => p.size * p.size).sum
    val sumSquareSetsPerPartition = partitions.map(_.map(p => p.size * p.size).sum).max
    Seq(maxPortsPerPartition, maxSetsPerPartition, sumSquarePortsPerPartition, sumSquareSetsPerPartition)
  }

  def partitionPort(port: Port, bit: BigInt): (Port, Port) = {
    val addr_a = AddressSet(0, ~bit)
    val addr_b = AddressSet(bit, ~bit)
    // The addresses were sorted, so the filtered addresses are still sorted
    val subset_a = port.filter(_.overlaps(addr_a))
    val subset_b = port.filter(_.overlaps(addr_b))
    (subset_a, subset_b)
  }

  def partitionPorts(ports: Ports, bit: BigInt): (Ports, Ports) = {
    val partitioned_ports = ports.map(p => partitionPort(p, bit))
    // because partitionPort dropped AddresSets, the ports might no longer be sorted
    val case_a_ports = partitioned_ports.map(_._1).filter(!_.isEmpty).sorted(portOrder)
    val case_b_ports = partitioned_ports.map(_._2).filter(!_.isEmpty).sorted(portOrder)
    (case_a_ports, case_b_ports)
  }

  def partitionPartitions(partitions: Partitions, bit: BigInt): Partitions = {
    val partitioned_partitions = partitions.map(p => partitionPorts(p, bit))
    val case_a_partitions = partitioned_partitions.map(_._1).filter(!_.isEmpty)
    val case_b_partitions = partitioned_partitions.map(_._2).filter(!_.isEmpty)
    val new_partitions = (case_a_partitions ++ case_b_partitions).sorted(partitionOrder)
    // Prevent combinational memory explosion; if two partitions are equal, keep only one
    // Note: AddressSets in a port are sorted, and ports in a partition are sorted.
    // This makes it easy to structurally compare two partitions for equality
    val keep = (new_partitions.init zip new_partitions.tail) filter { case (a, b) => partitionOrder.compare(a, b) != 0 } map {
      _._2
    }
    new_partitions.head +: keep
  }

  // requirement: ports have sorted addresses and are sorted lexicographically
  val debug = false

  def recurse(partitions: Partitions, bits: Seq[BigInt]): Seq[BigInt] = {
    if (partitions.map(_.size <= 1).reduce(_ && _)) Seq() else {
      if (debug) {
        println("Partitioning:")
        partitions.foreach { partition =>
          println("  Partition:")
          partition.foreach { port =>
            print("   ")
            port.foreach { a => print(s" ${a}") }
            println("")
          }
        }
      }
      val candidates = bits.map { bit =>
        val result = partitionPartitions(partitions, bit)
        val score = bitScore(result)
        if (debug)
          println("  For bit %x, %s".format(bit, score.toString))
        (score, bit, result)
      }
      val (bestScore, bestBit, bestPartitions) = candidates.min(Ordering.by[(Seq[Int], BigInt, Partitions), Iterable[Int]](_._1))
      if (debug) println("=> Selected bit 0x%x".format(bestBit))
      bestBit +: recurse(bestPartitions, bits.filter(_ != bestBit))
    }
  }
}

object MaskGen {
  def apply(addr_lo: UInt, lgSize: UInt, beatBytes: Int, groupBy: Int = 1): UInt = {
    require(groupBy >= 1 && beatBytes >= groupBy)
    require(isPow2(beatBytes) && isPow2(groupBy))
    val lgBytes = log2Ceil(beatBytes)
    val sizeOH = UIntToOH(lgSize | 0.U(log2Up(beatBytes).W), log2Up(beatBytes)) | (groupBy * 2 - 1).U

    def helper(i: Int): Seq[(Bool, Bool)] = {
      if (i == 0) {
        Seq((lgSize >= lgBytes.asUInt, true.B))
      } else {
        val sub = helper(i - 1)
        val size = sizeOH(lgBytes - i)
        val bit = addr_lo(lgBytes - i)
        val nbit = !bit
        Seq.tabulate(1 << i) { j =>
          val (sub_acc, sub_eq) = sub(j / 2)
          val eq = sub_eq && (if (j % 2 == 1) bit else nbit)
          val acc = sub_acc || (size && eq)
          (acc, eq)
        }
      }
    }

    if (groupBy == beatBytes) 1.U else
      Cat(helper(lgBytes - log2Ceil(groupBy)).map(_._1).reverse)
  }
}