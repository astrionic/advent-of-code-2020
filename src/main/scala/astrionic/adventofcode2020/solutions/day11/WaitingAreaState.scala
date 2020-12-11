package astrionic.adventofcode2020.solutions.day11

import astrionic.adventofcode2020.solutions.day11.Helpers.Tuple2IntExtensions
import astrionic.adventofcode2020.solutions.day11.SeatStatus.Seat

private[day11] class WaitingAreaState(
    private val grid: Array[Array[GridLocation]]
) {
  private val height: Int = grid.length
  private val width: Int = if(grid.length > 0) grid(0).length else 0

  private val directions: Seq[(Int, Int)] =
    (for(x <- -1 to 1; y <- -1 to 1) yield (x, y)).filter(_ != (0, 0))

  def getStatus(rowAndCol: (Int, Int)): Option[Seat] = {
    val (row, col) = rowAndCol
    if((0 until height contains row) && (0 until width contains col))
      Some(grid(row)(col).status)
    else
      None
  }

  private def getAdjacent(rowAndCol: (Int, Int)): List[Seat] =
    directions.flatMap(pos => getStatus(pos + rowAndCol)).toList

  def set(rowAndCol: (Int, Int), value: Seat): Unit = {
    val (row, col) = rowAndCol
    if((0 to height contains row) && (0 until width contains col)) {
      grid(row)(col) = GridLocation(value, hasChanged = true)
    }
  }

  def numOccupied: Int =
    grid.flatten.count(_.status == SeatStatus.Occupied)

  def hasChanged: Boolean =
    grid.flatten.count(_.hasChanged) > 0

  def calculateNext(): WaitingAreaState = {
    val newState = new WaitingAreaState(Array.ofDim[GridLocation](height, width))
    for(row <- 0 until height; col <- 0 until width) {
      val seat = grid(row)(col)
      val adj = getAdjacent((row, col))
      if(seat.status == SeatStatus.Empty && adj.count(_ == SeatStatus.Occupied) == 0) {
        newState.grid(row)(col) = GridLocation(SeatStatus.Occupied, hasChanged = true)
      } else if(seat.status == SeatStatus.Occupied && adj.count(_ == SeatStatus.Occupied) >= 4) {
        newState.grid(row)(col) = GridLocation(SeatStatus.Empty, hasChanged = true)
      } else {
        newState.grid(row)(col) = seat.copy(hasChanged = false)
      }
    }
    newState
  }
}
