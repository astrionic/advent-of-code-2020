package astrionic.adventofcode2020.solutions.day05

import astrionic.adventofcode2020.framework.AdventSolution

object Day05 extends AdventSolution {

  override def solvePart1(input: String): String = {
    val passes = parseInput(input)
    passes.map(_.getId).max.toString
  }

  override def solvePart2(input: String): String = {
    val passes = parseInput(input)
    val idsSorted = passes.map(_.getId).sorted
    val offset = idsSorted.head
    val firstMismatch = idsSorted.zipWithIndex
      .filter(id => id._1 != id._2 + offset)
      .head
      ._1
    val myPass = firstMismatch - 1
    myPass.toString
  }

  private def parseInput(input: String): Array[BoardingPass] = {
    input
      .replaceAll("[FL]", "0")
      .replaceAll("[BR]", "1")
      .split('\n')
      .map(b => BoardingPass.fromBinaryString(b))
  }

  private case class BoardingPass(row: Int, col: Int) {
    def getId: Int = row * 8 + col
  }

  private object BoardingPass {
    def fromBinaryString(b: String): BoardingPass = {
      val rowBinary = b.slice(0, 7)
      val colBinary = b.slice(7, 10)

      val rowDecimal = Integer.parseInt(rowBinary, 2)
      val colDecimal = Integer.parseInt(colBinary, 2)

      BoardingPass(rowDecimal, colDecimal)
    }
  }
}
