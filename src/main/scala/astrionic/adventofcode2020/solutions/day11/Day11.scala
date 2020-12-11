package astrionic.adventofcode2020.solutions.day11

import astrionic.adventofcode2020.framework.AdventSolution
import astrionic.adventofcode2020.solutions.day11.SeatStatus.SeatStatus

//noinspection DuplicatedCode
object Day11 extends AdventSolution {
  override def solvePart1(input: String): String = {
    val initial = parseInput(input).map(_.map(GridLocation(_, hasChanged = true)))

    var state = new WaitingAreaState(initial)
    while(state.hasChanged) {
      state = state.calculateNextPart1()
    }
    state.numOccupied.toString
  }

  override def solvePart2(input: String): String = {
    val initial = parseInput(input).map(_.map(GridLocation(_, hasChanged = true)))
    var state = new WaitingAreaState(initial)
    while(state.hasChanged) {
      state = state.calculateNextPart2()
    }
    state.numOccupied.toString
  }

  private def parseInput(input: String): Array[Array[SeatStatus]] = {
    input
      .split('\n')
      .map(
        _.toCharArray.map(parseChar)
      )
  }

  private def parseChar(c: Char): SeatStatus = c match {
    case 'L' => SeatStatus.Empty
    case '.' => SeatStatus.Floor
    case _   => throw new Exception("Invalid input")
  }
}
