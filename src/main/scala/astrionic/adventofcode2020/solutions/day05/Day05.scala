package astrionic.adventofcode2020.solutions.day05

import astrionic.adventofcode2020.framework.AdventSolution

object Day05 extends AdventSolution {

  writeSolution = true
  executePart = ExecutePart.Two

  override def solvePart1(input: String): String = {
    val binary = input
      .replaceAll("[FL]", "0")
      .replaceAll("[BR]", "1")

    val ids = binary
      .split('\n')
      .map(b => {
        val rowBinary = b.slice(0, 7)
        val colBinary = b.slice(7, 10)

        val rowDecimal = Integer.parseInt(rowBinary, 2)
        val colDecimal = Integer.parseInt(colBinary, 2)

        rowDecimal * 8 + colDecimal
      })

    ids.max.toString
  }

  override def solvePart2(input: String): String = {
    ???
  }
}
