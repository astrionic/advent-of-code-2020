package astrionic.adventofcode2020.solutions.day14

import astrionic.adventofcode2020.framework.AdventSolution

object Day14 extends AdventSolution {

  // writeSolution = true
  executePart = ExecutePart.One

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
    ???
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

  private def toBinary(n: Long): String = n match {
    case 1 | 0 => n.toString
    case _     => s"${toBinary(n / 2)}${n % 2}"
  }

  private def toDecimal(binary: String): Long =
    java.lang.Long.parseLong(binary, 2)
}
