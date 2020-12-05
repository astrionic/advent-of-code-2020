package astrionic.adventofcode2020.solutions.day05

import astrionic.adventofcode2020.framework.AdventSolution

object Day05 extends AdventSolution {

  override def solvePart1(input: String): String = parseIds(input).max.toString

  override def solvePart2(input: String): String = {
    val idsSorted = parseIds(input).sorted
    val offset = idsSorted.head
    val firstMismatch = idsSorted.zipWithIndex
      .filter(id => id._1 != id._2 + offset)
      .head
      ._1
    val myPass = firstMismatch - 1
    myPass.toString
  }

  private def parseIds(input: String): Array[Int] =
    input
      .replaceAll("[FL]", "0")
      .replaceAll("[BR]", "1")
      .split('\n')
      .map(b => Integer.parseInt(b, 2))
}
