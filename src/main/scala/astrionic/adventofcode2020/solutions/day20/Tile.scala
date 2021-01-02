package astrionic.adventofcode2020.solutions.day20

import astrionic.adventofcode2020.solutions.day20.Tile.encodeEdge

private[day20] case class Tile(id: Int, pattern: List[String]) {
  def getEdgeCodes: List[Int] = {
    val edges = getLine(0) :: getLine(8) :: getCol(0) :: getCol(8) :: Nil
    val withMirrored: List[String] = edges ++ edges.map(_.reverse)
    withMirrored
      .map(_.map(_ == '#'))
      .map(encodeEdge(_))
  }

  def getLine(i: Int): String = pattern(i)

  def getCol(i: Int): String = pattern.map(_(i)).mkString

  def rotated(): Tile = ???
  def mirrored(): Tile = ???
}

private[day20] object Tile {
  def encodeEdge(edge: Seq[Boolean]): Int = edge match {
    case Nil => 0
    case nonEmpty =>
      val binaryString = nonEmpty.map(if(_) "1" else "0").mkString
      Integer.parseInt(binaryString, 2)
  }

}
