package astrionic.adventofcode2020.solutions.day20

/**
 * @param sides The tile's sides in clockwise order, starting at the top. The char
 */
case class SideOnlyTile(sides: List[Char]) {
  def top = sides(0)
}
