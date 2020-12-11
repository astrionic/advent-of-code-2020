package astrionic.adventofcode2020.solutions.day11

import astrionic.adventofcode2020.solutions.day11.Helpers.Tuple2IntExtensions
import astrionic.adventofcode2020.solutions.day11.SeatStatus.SeatStatus

private[day11] class WaitingAreaState(
    private val grid: Array[Array[GridLocation]]
) {
  private val height: Int = grid.length
  private val width: Int = if(grid.length > 0) grid(0).length else 0

  private val directions: Seq[(Int, Int)] =
    (for(x <- -1 to 1; y <- -1 to 1) yield (x, y)).filter(_ != (0, 0))

  def getStatus(rowAndCol: (Int, Int)): Option[SeatStatus] = {
    val (row, col) = rowAndCol
    if((0 until height contains row) && (0 until width contains col))
      Some(grid(row)(col).status)
    else
      None
  }

  private def getAdjacent(rowAndCol: (Int, Int)): List[SeatStatus] =
    directions.flatMap(pos => getStatus(pos + rowAndCol)).toList

  def set(rowAndCol: (Int, Int), value: SeatStatus): Unit = {
    val (row, col) = rowAndCol
    if((0 to height contains row) && (0 until width contains col)) {
      grid(row)(col) = GridLocation(value, hasChanged = true)
    }
  }

  private def getVisible(rowAndCol: (Int, Int)): List[SeatStatus] =
    directions.flatMap(findVisible(rowAndCol, _)).toList

  private def findVisible(rowAndCol: (Int, Int), step: (Int, Int)): Option[SeatStatus] = {
    var pos = rowAndCol + step
    while((0 until height contains pos._1) && (0 until width contains pos._2)) {
      if(grid(pos._1)(pos._2).status != SeatStatus.Floor)
        return Some(grid(pos._1)(pos._2).status)
      pos += step
    }
    None
  }

  def numOccupied: Int =
    grid.flatten.count(_.status == SeatStatus.Occupied)

  def hasChanged: Boolean =
    grid.flatten.count(_.hasChanged) > 0

  def calculateNextPart1(): WaitingAreaState = {
    val nextState = new WaitingAreaState(Array.ofDim[GridLocation](height, width))
    for(row <- 0 until height; col <- 0 until width) {
      val seat = grid(row)(col)
      val adj = getAdjacent((row, col))
      if(seat.status == SeatStatus.Empty && adj.count(_ == SeatStatus.Occupied) == 0) {
        nextState.grid(row)(col) = GridLocation(SeatStatus.Occupied, hasChanged = true)
      } else if(seat.status == SeatStatus.Occupied && adj.count(_ == SeatStatus.Occupied) >= 4) {
        nextState.grid(row)(col) = GridLocation(SeatStatus.Empty, hasChanged = true)
      } else {
        nextState.grid(row)(col) = seat.copy(hasChanged = false)
      }
    }
    nextState
  }

  def calculateNextPart2(): WaitingAreaState = {
    val nextState = new WaitingAreaState(Array.ofDim[GridLocation](height, width))
    for(row <- 0 until height; col <- 0 until width) {
      val seat = grid(row)(col)
      val adj = getVisible((row, col))
      if(seat.status == SeatStatus.Empty && adj.count(_ == SeatStatus.Occupied) == 0) {
        nextState.grid(row)(col) = GridLocation(SeatStatus.Occupied, hasChanged = true)
      } else if(seat.status == SeatStatus.Occupied && adj.count(_ == SeatStatus.Occupied) >= 5) {
        nextState.grid(row)(col) = GridLocation(SeatStatus.Empty, hasChanged = true)
      } else {
        nextState.grid(row)(col) = seat.copy(hasChanged = false)
      }
    }
    nextState
  }
}
