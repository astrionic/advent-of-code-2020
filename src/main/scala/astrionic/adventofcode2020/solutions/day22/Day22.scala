package astrionic.adventofcode2020.solutions.day22

import astrionic.adventofcode2020.framework.AdventSolution

object Day22 extends AdventSolution {

  // writeSolution = true
  executePart = ExecutePart.One

  override def solvePart1(input: String): String = {
    var (p1, p2) = parseInput(input)

    while(p1.nonEmpty && p2.nonEmpty) {
      if(p1.head > p2.head) {
        p1 = p1.tail ++ List(p1.head, p2.head)
        p2 = p2.tail
      } else {
        p2 = p2.tail ++ List(p2.head, p1.head)
        p1 = p1.tail
      }
    }

    val winningDeck = if(p1.nonEmpty) p1 else p2
    winningDeck
      .zip((1 to winningDeck.length).reverse)
      .map(x => x._1 * x._2)
      .sum
      .toString
  }

  override def solvePart2(input: String): String = {
    ???
  }

  private def parseInput(input: String): (List[Int], List[Int]) = {
    val regex = """Player 1:\n([\d\n]+)\nPlayer 2:\n([\d\n]+)""".r
    input match {
      case regex(p1, p2) => (p1.split('\n').map(_.toInt).toList, p2.split('\n').map(_.toInt).toList)
      case _             => throw new Exception
    }
  }
}
