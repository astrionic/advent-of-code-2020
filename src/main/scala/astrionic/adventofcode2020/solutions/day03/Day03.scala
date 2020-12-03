package astrionic.adventofcode2020.solutions.day03

import astrionic.adventofcode2020.framework.AdventSolution

object Day03 extends AdventSolution {

  override def solvePart1(input: String): String = {
    val plan = parseInput(input) // plan(row, top down)(col, left to right)
    val numRows = plan.length
    val numCols = plan(0).length

    (0 until numRows)
      .map(row => plan(row)((row * 3) % numCols))
      .count(_ == true)
      .toString
  }

  override def solvePart2(input: String): String = {
    val plan = parseInput(input) // plan(row, top down)(col, left to right)
    val numRows = plan.length
    val numCols = plan(0).length

    val slopes = List((1, 1), (1, 3), (1, 5), (1, 7), (2, 1))

    val createEntirePath = (slope: (Int, Int)) => {
      (0 until numRows by slope._1)
        .zip(0 until numRows * slope._2 by slope._2)
    }

    slopes
      .map(
        createEntirePath(_)
          .map(pos => plan(pos._1)(pos._2 % numCols))
          .count(_ == true): Long
      )
      .product
      .toString
  }

  private def parseInput(input: String): Array[Array[Boolean]] = {
    val lines = input.split('\n')
    lines.map(_.toCharArray.map(_ == '#'))
  }
}
