package astrionic.adventofcode2020.solutions.day15

import astrionic.adventofcode2020.framework.AdventSolution

import scala.collection.mutable

object Day15 extends AdventSolution {

  override def solvePart1(input: String): String = solve(input, 2020)

  // Takes about 250 times as long as part 1 (around 9 seconds on my current machine)
  override def solvePart2(input: String): String = solve(input, 30000000)

  def parseInput(input: String): List[Int] =
    input.split(',').flatMap(_.toIntOption).toList

  def solve(input: String, turns: Int): String = {
    val startingNumbers = parseInput(input)

    val map = mutable.Map[Int, Int]()
    map.addAll(
      startingNumbers
        .slice(0, startingNumbers.length - 1)
        .zip(1 until startingNumbers.length)
        .toMap
    )
    var previousNum = startingNumbers.last
    for(turn <- startingNumbers.length + 1 to turns) {
      val currentNum = map.get(previousNum) match {
        case Some(n) => (turn - 1) - n
        case None    => 0
      }
      map.addOne(previousNum -> (turn - 1))
      previousNum = currentNum
    }
    previousNum.toString
  }
}
