package astrionic.adventofcode2020.solutions.day24

import astrionic.adventofcode2020.solutions.day24.Dir.Dir
import astrionic.adventofcode2020.solutions.day24.TileFloor.move
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
}

private[day24] object TileFloor {
  private def move(loc: (Int, Int, Int), dir: Dir): (Int, Int, Int) = dir match {
    case Dir.E  => loc + (1, -1, 0)
    case Dir.SE => loc + (0, -1, 1)
    case Dir.SW => loc + (-1, 0, 1)
    case Dir.W  => loc + (-1, 1, 0)
    case Dir.NW => loc + (0, 1, -1)
    case Dir.NE => loc + (1, 0, -1)
  }
}
