package astrionic.adventofcode2020.solutions.day23

import astrionic.adventofcode2020.framework.AdventSolution

object Day23 extends AdventSolution {

  // writeSolution = true
  executePart = ExecutePart.One

  override def solvePart1(input: String): String = {
    var cups = parseInput(input)
    for(_ <- 0 until 100) {
      println(cups)
      cups = move(cups)
    }
    val (front, back) = cups.splitAt(cups.indexOf(1))
    (back ++ front).tail.mkString
  }

  override def solvePart2(input: String): String = {
    ???
  }

  private def parseInput(input: String): List[Int] = {
    val regex = """\d""".r
    regex.findAllIn(input).map(_.toInt).toList
  }

  private def move(cups: List[Int]): List[Int] = {
    // Current cup is always in front
    val removed = cups.slice(1, 4)
    val remaining = cups.head :: cups.slice(4, cups.length)
    val destCupIdx = findDestCupIdx(cups.head, remaining)
    val (front, back) = remaining.splitAt(destCupIdx + 1)
    val newOrder = front ++ removed ++ back // Insert removed cups
    newOrder.tail :+ newOrder.head // Select new current cup by moving current to the back
  }

  private def findDestCupIdx(currentCupValue: Int, cups: List[Int]): Int = {
    val destinationCupValue =
      if(currentCupValue <= cups.min) cups.max
      else cups.filter(_ < currentCupValue).max
    cups.indexOf(destinationCupValue)
  }
}
