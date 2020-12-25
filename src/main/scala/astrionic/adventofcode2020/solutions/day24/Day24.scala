package astrionic.adventofcode2020.solutions.day24

import astrionic.adventofcode2020.framework.AdventSolution
import astrionic.adventofcode2020.solutions.day24.Direction.Dir

object Day24 extends AdventSolution {

  override def solvePart1(input: String): String = {
    val paths = parseInput(input)

    val floor = new TileFloor()
    floor.flipAll(paths)

    floor.numBlackTiles.toString
  }

  override def solvePart2(input: String): String = {
    val paths = parseInput(input)

    val floor = new TileFloor()
    floor.flipAll(paths)

    floor.simulateGenerations(100)

    floor.numBlackTiles.toString
  }

  private def parseInput(input: String): List[List[Dir]] = {
    input
      .split("\n")
      .map(parseLine)
      .toList
  }

  private val directionRegex = "e|se|sw|w|nw|ne".r

  private def parseLine(line: String): List[Dir] = {
    directionRegex
      .findAllIn(line)
      .map {
        case "e"  => Direction.E
        case "se" => Direction.SE
        case "sw" => Direction.SW
        case "w"  => Direction.W
        case "nw" => Direction.NW
        case "ne" => Direction.NE
        case _    => throw new Exception
      }
      .toList
  }
}
