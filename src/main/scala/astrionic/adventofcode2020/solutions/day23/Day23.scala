package astrionic.adventofcode2020.solutions.day23

import astrionic.adventofcode2020.framework.AdventSolution

object Day23 extends AdventSolution {

  override def solvePart1(input: String): String = {
    val cups = parseInput(input)
    val cc = CupCollection.fromList(cups)
    cc.move(100)
    cc.resultPart1
  }

  override def solvePart2(input: String): String = {
    val cups = parseInput(input)
    val cc = CupCollection.fromList(cups, 1_000_000)
    cc.move(10_000_000)
    cc.resultPart2.toString
  }

  private def parseInput(input: String): List[Int] = {
    val regex = """\d""".r
    regex.findAllIn(input).map(_.toInt).toList
  }
}
