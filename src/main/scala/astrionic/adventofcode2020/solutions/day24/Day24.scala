package astrionic.adventofcode2020.solutions.day24

import astrionic.adventofcode2020.framework.AdventSolution
import astrionic.adventofcode2020.solutions.day24.Day24.Dir.Dir
import astrionic.adventofcode2020.solutions.helpers.Helpers.Tuple3IntExtensions

object Day24 extends AdventSolution {

  // writeSolution = true
  executePart = ExecutePart.One

  override def solvePart1(input: String): String = {
    val paths = parseInput(input)
    var black = Set[(Int, Int, Int)]()

    def flip(tile: (Int, Int, Int)): Unit = {
      if(black.contains(tile)) {
        black -= tile
      } else {
        black += tile
      }
    }

    val start = (0, 0, 0)
    for(path <- paths) {
      val tile = path.foldLeft(start)((loc, dir) => move(loc, dir))
      flip(tile)
    }

    black.size.toString
  }

  override def solvePart2(input: String): String = {
    ???
  }

  private def parseInput(input: String): List[List[Dir]] = {
    val regex = "e|se|sw|w|nw|ne".r
    input
      .split("\n")
      .map(
        regex
          .findAllIn(_)
          .map {
            case "e"  => Dir.E
            case "se" => Dir.SE
            case "sw" => Dir.SW
            case "w"  => Dir.W
            case "nw" => Dir.NW
            case "ne" => Dir.NE
            case _    => throw new Exception
          }
          .toList
      )
      .toList
  }

  private[day24] object Dir extends Enumeration {
    type Dir = Value
    val E, SE, SW, W, NW, NE = Value
  }

  private def move(loc: (Int, Int, Int), dir: Dir): (Int, Int, Int) = dir match {
    case Dir.E  => loc + (1, -1, 0)
    case Dir.SE => loc + (0, -1, 1)
    case Dir.SW => loc + (-1, 0, 1)
    case Dir.W  => loc + (-1, 1, 0)
    case Dir.NW => loc + (0, 1, -1)
    case Dir.NE => loc + (1, 0, -1)

  }
}
