package astrionic.adventofcode2020.solutions.day13

import astrionic.adventofcode2020.framework.AdventSolution

object Day13 extends AdventSolution {

  writeSolution = true
  executePart = ExecutePart.One

  override def solvePart1(input: String): String = {
    val (earliestDeparture, idsWithXs) = parseInput(input)
    val ids = idsWithXs
      .filter(id => id.matches("^\\d+\\z"))
      .map(_.toInt)

    val earliestBuses = ids.map(id => (id, ((earliestDeparture / id) + 1) * id))
    val earliestBus = earliestBuses.minBy(_._2)
    val waitingTime = earliestBus._2 - earliestDeparture

    (earliestBus._1 * waitingTime).toString
  }

  override def solvePart2(input: String): String = {
    ???
  }

  private def parseInput(input: String): (Int, Array[String]) = {
    val lines = input.split('\n')
    val earliestDeparture = lines(0).toInt
    val busIds = lines(1).split(',')
    (earliestDeparture, busIds)
  }
}
