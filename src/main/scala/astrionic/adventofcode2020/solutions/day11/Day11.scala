package astrionic.adventofcode2020.solutions.day11

import astrionic.adventofcode2020.framework.AdventSolution

object Day11 extends AdventSolution {

  // writeSolution = true
  executePart = ExecutePart.One

  override def solvePart1(input: String): String = {
    val initial = parseInput(input)
    val rows = initial.length
    val cols = initial(0).length

    var changes = 1
    var state = initial
    while(changes != 0) {
      import Seat._
      val newState = Array.ofDim[Seat](rows, cols)
      changes = 0
      for(row <- 0 until rows; col <- 0 until cols) {
        val seat = state(row)(col)
        val adj = getAdjacent(state, row, col)
        if(seat == Seat.Empty && adj.count(_ == Seat.Occupied) == 0) {
          newState(row)(col) = Seat.Occupied
          changes += 1
        } else if(seat == Seat.Occupied && adj.count(_ == Seat.Occupied) >= 4) {
          newState(row)(col) = Seat.Empty
          changes += 1
        } else {
          newState(row)(col) = seat
        }
      }
      state = newState
    }
    state.flatten.count(_ == Seat.Occupied).toString
  }

  override def solvePart2(input: String): String = {
    ???
  }

  private object Seat extends Enumeration {
    type Seat = Value
    val Floor, Empty, Occupied = Value
  }

  import Seat._

  private def parseInput(input: String): Array[Array[Seat]] = {
    input
      .split('\n')
      .map(
        _.toCharArray.map(parseChar)
      )
  }

  private def parseChar(c: Char): Seat = c match {
    case 'L' => Seat.Empty
    case '.' => Seat.Floor
    case _   => throw new Exception("Invalid input")
  }

  private def getAdjacent(state: Array[Array[Seat]], row: Int, col: Int): List[Seat] = {
    val rows = state.length
    val cols = state(0).length

    var adjacent = List[Seat]()
    if(row > 0) {
      adjacent ++= state(row - 1).slice(Math.max(0, col - 1), Math.min(cols, col + 2))
    }
    if(col > 0) adjacent ::= state(row)(col - 1)
    if(col < cols - 1) adjacent ::= state(row)(col + 1)
    if(row < rows - 1) {
      adjacent ++= state(row + 1).slice(Math.max(0, col - 1), Math.min(cols, col + 2))
    }
    adjacent
  }
}
