package astrionic.adventofcode2020.solutions.day20

import astrionic.adventofcode2020.framework.AdventSolution

object Day20 extends AdventSolution {

  writeSolution = true
  executePart = ExecutePart.One

  override def solvePart1(input: String): String = {
    // 0. Parse input
    // 1. Create object that represents tiles
    //    - Represent rotation somehow
    // 2. Write match function
    // 3. Match all tiles. Find a way to represent the completed (or in progress) picture

    val tiles = parseInput(input)

    println(s"Number of tiles: ${tiles.length}")
    val edges = tiles
      .flatMap(t => List(t.getCol(0), t.getCol(9), t.getLine(0), t.getLine(9)))
      .flatMap(t => List(t, t.reverse))
    println(s"Number of edges:\t\t${edges.length}")
    println(s"Number of distinct edges:\t\t${edges.distinct.length}")
    ""
  }

  override def solvePart2(input: String): String = {
    ???
  }

  private def parseInput(input: String): List[Tile] = {
    val regex = """Tile (\d{4}):\n((?:[.#]{10}\n){10})""".r
    regex
      .findAllMatchIn(input)
      .map(m => Tile(m.subgroups.head.toInt, m.subgroups(1).split('\n').toList))
      .toList
  }
}
