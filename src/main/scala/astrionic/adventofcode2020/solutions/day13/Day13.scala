package astrionic.adventofcode2020.solutions.day13

import astrionic.adventofcode2020.framework.AdventSolution

object Day13 extends AdventSolution {
  override def solvePart1(input: String): String = {
    val (earliestDeparture, idsWithXs) = parseInput(input)
    val ids: Array[Int] = idsWithXs.flatten // Get rid of 'x' entries

    val earliestBuses = ids.map(id => (id, ((earliestDeparture / id) + 1) * id))
    val earliestBus = earliestBuses.minBy(_._2)
    val waitingTime = earliestBus._2 - earliestDeparture

    (earliestBus._1 * waitingTime).toString
  }

  override def solvePart2(input: String): String = {
    val (_, idPositions: Array[Option[Int]]) = parseInput(input)
    val ids: Array[(Int, Int)] = idPositions.zipWithIndex.flatMap(extractOption)

    var start = 0L
    var step = 1L

    for((id, i) <- ids) {
      while((start + i) % id != 0) {
        start += step
      }
      step *= id
    }
    
    start.toString
  }

  private def parseInput(input: String): (Int, Array[Option[Int]]) = {
    val lines = input.split('\n')
    val earliestDeparture = lines(0).toInt
    val busIds = lines(1).split(',').map(_.toIntOption)
    (earliestDeparture, busIds)
  }

  private def extractOption[A, B](x: (Option[A], B)): Option[(A, B)] = x match {
    case (Some(a), b) => Some((a, b))
    case (None, _)    => None
  }
}
