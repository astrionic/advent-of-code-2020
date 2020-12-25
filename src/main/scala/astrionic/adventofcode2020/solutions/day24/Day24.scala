package astrionic.adventofcode2020.solutions.day24

import astrionic.adventofcode2020.framework.AdventSolution
import astrionic.adventofcode2020.solutions.day24.Dir.Dir

object Day24 extends AdventSolution {

  // writeSolution = true
  executePart = ExecutePart.One

  override def solvePart1(input: String): String = {
    val paths = parseInput(input)

    val floor = new TileFloor()
    floor.flipAll(paths)

    floor.numBlackTiles.toString
  }

  override def solvePart2(input: String): String = {
    ???
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
        case "e"  => Dir.E
        case "se" => Dir.SE
        case "sw" => Dir.SW
        case "w"  => Dir.W
        case "nw" => Dir.NW
        case "ne" => Dir.NE
        case _    => throw new Exception
      }
      .toList
  }
}
