package astrionic.adventofcode2020.solutions.day10

import astrionic.adventofcode2020.framework.AdventSolution

import scala.collection.immutable.HashMap

object Day10 extends AdventSolution {

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
    val adapterJoltages = parseInput(input).sorted
    val builtIn = adapterJoltages.max + 3
    val adaptersWithBuiltIn = adapterJoltages :+ builtIn

    var possibilities = HashMap[Int, Long](
      0 -> 1 // Add outlet
    )

    for(adapter <- adaptersWithBuiltIn) {
      val min = Math.max(0, adapter - 3)
      val max = adapter - 1
      val n = (min to max).map(possibilities.getOrElse(_, 0L)).sum
      possibilities += (adapter -> n)
    }

    possibilities.get(builtIn) match {
      case Some(n) => n.toString
      case None    => "No solution found"
    }
  }

  private def parseInput(input: String): List[Int] =
    input.split('\n').map(_.toInt).toList
}
