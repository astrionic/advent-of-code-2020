package astrionic.adventofcode2020.solutions.day14

import astrionic.adventofcode2020.framework.AdventSolution

object Day14 extends AdventSolution {

  override def solvePart1(input: String): String = {
    val program: Seq[Instr] = parseInput(input)

    var mask = "X".repeat(36)
    var mem = Map[Int, Long]()
    for(instr <- program) instr match {
      case UpdateMask(newMask) => mask = newMask
      case Write(addr, value)  => mem += (addr -> applyMask(value, mask))
    }

    mem.values.sum.toString
  }

  override def solvePart2(input: String): String = {
    val program: Seq[Instr] = parseInput(input)

    var mask = "X".repeat(36)
    var mem = Map[Long, Long]()
    for(instr <- program) instr match {
      case UpdateMask(newMask) => mask = newMask
      case Write(addr, value) => {
        val masked = applyMaskPart2(addr, mask)
        val addresses = createPossibleAddresses(masked).map(toDecimal)
        addresses.foreach { addr => mem += (addr -> value) }
      }
    }
    mem.values.sum.toString
  }

  private trait Instr
  private case class UpdateMask(mask: String) extends Instr
  private case class Write(addr: Int, value: Long) extends Instr

  private def parseInput(input: String): List[Instr] =
    input.split('\n').flatMap(parseLine).toList

  private val maskInstrPattern = "^mask = [01X]{36}\\z".r
  private val writeInstrPattern = "^mem\\[\\d+] = \\d+\\z".r

  private def parseLine(line: String): Option[Instr] = line match {
    case s if maskInstrPattern matches s =>
      Some(UpdateMask(s.substring(7)))
    case s if writeInstrPattern matches s =>
      val parts = s.split("] = ")
      val addr = parts(0).substring(4).toInt
      val value = parts(1).toLong
      Some(Write(addr, value))
    case _ =>
      None
  }

  private def applyMask(value: Long, mask: String): Long = {
    val valueBinary = toBinary(value)
    val resultBinary = valueBinary.reverse
      .zipAll(mask.reverse, '0', 'X')
      .map {
        case (valueBit, 'X') => valueBit
        case (_, maskBit)    => maskBit
      }
      .reverse
      .mkString("")
    toDecimal(resultBinary)
  }

  private def applyMaskPart2(addr: Long, mask: String): String = {
    val addrBinary = toBinary(addr)
    addrBinary.reverse
      .zipAll(mask.reverse, '0', '0')
      .map {
        case (_, '1')     => '1'
        case (_, 'X')     => 'X'
        case (addrBit, _) => addrBit
      }
      .reverse
      .mkString("")
  }

  private def toBinary(decimal: Long): String = decimal match {
    case 1 | 0 => decimal.toString
    case _     => s"${toBinary(decimal / 2)}${decimal % 2}"
  }

  private def toDecimal(binary: String): Long =
    java.lang.Long.parseLong(binary, 2)

  private def createPossibleAddresses(addr: String): List[String] = {
    if(addr.contains('X')) {
      val zero = addr.replaceFirst("X", "0")
      val one = addr.replaceFirst("X", "1")
      createPossibleAddresses(zero) ++ createPossibleAddresses(one)
    } else {
      List(addr)
    }
  }
}
