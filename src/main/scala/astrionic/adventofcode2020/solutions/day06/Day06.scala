package astrionic.adventofcode2020.solutions.day06

import astrionic.adventofcode2020.framework.AdventSolution

object Day06 extends AdventSolution {

  override def solvePart1(input: String): String = {
    input
      .split("\n\n") // Split into groups
      .map(_.toSet - '\n') // Remove duplicate answers from each group and ignore newline
      .map(_.size) // Count answers for each group
      .sum
      .toString
  }

  override def solvePart2(input: String): String = {
    val groups: Array[Array[Set[Char]]] = input
      .split("\n\n")
      .map(_.split('\n').map(_.toSet))

    groups
      .map(_.reduce((a, b) => a intersect b)) // Find common answers in each group
      .map(_.size)
      .sum
      .toString
  }
}
