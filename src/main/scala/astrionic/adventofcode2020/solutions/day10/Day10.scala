package astrionic.adventofcode2020.solutions.day10

import astrionic.adventofcode2020.framework.AdventSolution

object Day10 extends AdventSolution {

  // writeSolution = true
  executePart = ExecutePart.One

  override def solvePart1(input: String): String = {
    val adapters = 0 :: parseInput(input).sorted
    val adaptersWithBuiltIn = adapters :+ adapters.max + 3

    val deltas = adapters.zipWithIndex
      .map(adapterWithIndex => {
        val (adapterJoltage, idx) = adapterWithIndex
        adaptersWithBuiltIn(idx + 1) - adapterJoltage
      })

    (deltas.count(_ == 1) * deltas.count(_ == 3)).toString
  }

  override def solvePart2(input: String): String = {
    ???
  }

  private def parseInput(input: String): List[Int] =
    input.split('\n').map(_.toInt).toList
}
