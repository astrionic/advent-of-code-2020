package astrionic.adventofcode2020.solutions.day12

import astrionic.adventofcode2020.solutions.day12.Ship.rotateDegreesClockwise
import astrionic.adventofcode2020.solutions.helpers.Helpers.Tuple2IntExtensions

import scala.annotation.tailrec

private[day12] case class Ship(
    pos: (Int, Int) = (0, 0),
    facing: (Int, Int) = (1, 0) // In part 2, this is the waypoint's position instead
) {
  def movePart1(cmd: Command): Ship = cmd match {
    case Command(Action.E, value) => copy(pos = pos + (value, 0))
    case Command(Action.W, value) => copy(pos = pos + (-value, 0))
    case Command(Action.S, value) => copy(pos = pos + (0, -value))
    case Command(Action.N, value) => copy(pos = pos + (0, value))
    case Command(Action.L, value) => copy(facing = turnClockwise(-value))
    case Command(Action.R, value) => copy(facing = turnClockwise(value))
    case Command(Action.F, value) => copy(pos = forward(value))
  }

  def movePart2(cmd: Command): Ship = cmd match {
    case Command(Action.E, value) => copy(facing = facing + (value, 0))
    case Command(Action.W, value) => copy(facing = facing + (-value, 0))
    case Command(Action.S, value) => copy(facing = facing + (0, -value))
    case Command(Action.N, value) => copy(facing = facing + (0, value))
    case Command(Action.L, value) => copy(facing = turnClockwise(-value))
    case Command(Action.R, value) => copy(facing = turnClockwise(value))
    case Command(Action.F, value) => copy(pos = forward(value))
  }

  private def forward(distance: Int): (Int, Int) = {
    pos + facing * distance
  }

  private def turnClockwise(degrees: Int): (Int, Int) = {
    rotateDegreesClockwise(facing, degrees)
  }

  def manhattanDistanceTo(otherPos: (Int, Int)): Int = {
    val d = pos - otherPos
    Math.abs(d._1) + Math.abs(d._2)
  }
}

private[day12] object Ship {
  private def rotate90Clockwise(vec: (Int, Int)): (Int, Int) = (vec._2, -vec._1)

  @tailrec
  private def rotate90Clockwise(vec: (Int, Int), times: Int): (Int, Int) = {
    var t = times % 4
    if(t < 0)
      t = t + 4
    if(t == 0)
      vec
    else
      rotate90Clockwise(rotate90Clockwise(vec), t - 1)
  }

  private def rotateDegreesClockwise(vec: (Int, Int), degrees: Int): (Int, Int) =
    rotate90Clockwise(vec, degrees / 90)

}
