package astrionic.adventofcode2020.solutions.day24

import astrionic.adventofcode2020.solutions.day24.Direction.Dir
import astrionic.adventofcode2020.solutions.day24.TileFloor.{getNeighbours, move, numBlackNeighbours}
import astrionic.adventofcode2020.solutions.helpers.Helpers.Tuple3IntExtensions

private[day24] class TileFloor {
  private var blackTiles = Set[(Int, Int, Int)]()
  private val start = (0, 0, 0)

  private def flip(tile: (Int, Int, Int)): Unit = {
    if(blackTiles.contains(tile)) {
      blackTiles -= tile
    } else {
      blackTiles += tile
    }
  }

  private def flip(path: List[Dir]): Unit = {
    val tile = path.foldLeft(start)((loc, dir) => move(loc, dir))
    flip(tile)
  }

  def flipAll(paths: List[List[Dir]]): Unit = paths.foreach(flip)

  def numBlackTiles: Int = blackTiles.size

  def simulateGeneration(): Unit = {
    val whiteTiles = blackTiles.flatMap(getNeighbours) diff blackTiles

    val stayBlack: Set[(Int, Int, Int)] = blackTiles.flatMap(tile => {
      val neighbours = numBlackNeighbours(tile, blackTiles)

      if(neighbours == 0 || neighbours > 2)
        None
      else
        Some(tile)
    })

    val turnBlack = whiteTiles.flatMap(tile => {
      val neighbours = numBlackNeighbours(tile, blackTiles)

      if(neighbours == 2)
        Some(tile)
      else
        None
    })

    blackTiles = stayBlack union turnBlack
  }

  def simulateGenerations(n: Int): Unit =
    for(_ <- 0 until n) simulateGeneration()
}

private[day24] object TileFloor {
  private def move(loc: (Int, Int, Int), dir: Dir): (Int, Int, Int) = dir match {
    case Direction.E  => loc + (1, -1, 0)
    case Direction.SE => loc + (0, -1, 1)
    case Direction.SW => loc + (-1, 0, 1)
    case Direction.W  => loc + (-1, 1, 0)
    case Direction.NW => loc + (0, 1, -1)
    case Direction.NE => loc + (1, 0, -1)
  }

  private def getNeighbours(loc: (Int, Int, Int)): Set[(Int, Int, Int)] =
    Direction.values.map(move(loc, _))

  private def numBlackNeighbours(tile: (Int, Int, Int), blackTiles: Set[(Int, Int, Int)]): Int =
    (getNeighbours(tile) intersect blackTiles).size

}
