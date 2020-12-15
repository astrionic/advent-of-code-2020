package astrionic.adventofcode2020.solutions.day15

import astrionic.adventofcode2020.framework.AdventSolution

object Day15 extends AdventSolution {

  override def solvePart1(input: String): String = solve(input, 2020)

  // Takes about 500 times longer than part 1, around 25 seconds on my machine
  override def solvePart2(input: String): String = solve(input, 30000000)

  def parseInput(input: String): List[Int] =
    input.split(',').flatMap(_.toIntOption).toList

  def solve(input: String, turns: Int): String = {
    val startingNumbers = parseInput(input)

    var map: Map[Int, Int] = startingNumbers
      .slice(0, startingNumbers.length - 1)
      .zip(1 until startingNumbers.length)
      .toMap
    var previousNum = startingNumbers.last
    for(turn <- startingNumbers.length + 1 to turns) {
      val currentNum = map.get(previousNum) match {
        case Some(n) => (turn - 1) - n
        case None    => 0
      }
      map += (previousNum -> (turn - 1))
      previousNum = currentNum
    }
    previousNum.toString
  }
}
