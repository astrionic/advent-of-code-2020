package astrionic.adventofcode2020.solutions.day11

import astrionic.adventofcode2020.framework.AdventSolution
import astrionic.adventofcode2020.solutions.day11.SeatStatus.Seat

object Day11 extends AdventSolution {

  // writeSolution = true
  executePart = ExecutePart.One

  override def solvePart1(input: String): String = {
    val initial = parseInput(input).map(_.map(GridLocation(_, hasChanged = true)))

    var state = new WaitingAreaState(initial)
    while(state.hasChanged) {
      state = state.calculateNext()
    }
    state.numOccupied.toString
  }

  override def solvePart2(input: String): String = {
    ???
  }

  private def parseInput(input: String): Array[Array[Seat]] = {
    input
      .split('\n')
      .map(
        _.toCharArray.map(parseChar)
      )
  }

  private def parseChar(c: Char): Seat = c match {
    case 'L' => SeatStatus.Empty
    case '.' => SeatStatus.Floor
    case _   => throw new Exception("Invalid input")
  }
}
