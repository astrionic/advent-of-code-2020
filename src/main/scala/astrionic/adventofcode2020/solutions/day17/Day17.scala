package astrionic.adventofcode2020.solutions.day17

import astrionic.adventofcode2020.framework.AdventSolution
import astrionic.adventofcode2020.solutions.helpers.Helpers.{Tuple3IntExtensions, Tuple4IntExtensions}

import scala.collection.immutable.HashSet

//noinspection DuplicatedCode
object Day17 extends AdventSolution {
  override def solvePart1(input: String): String = {
    var occupied = parseInput(input)
    for(_ <- 0 until 6) {
      val potential = occupied union occupied.flatMap(getNeighbourCoords)
      occupied = potential.flatMap(p => {
        val neighbours = getNeighbourCoords(p)
        val numActiveNeighbours = neighbours.count(occupied.contains(_))
        val active = occupied.contains(p)
        if(active && (2 to 3).contains(numActiveNeighbours)) {
          Some(p)
        } else if(!active && numActiveNeighbours == 3) {
          Some(p)
        } else {
          None
        }
      })
    }

    occupied.size.toString
  }

  override def solvePart2(input: String): String = {
    var occupied = parseInput(input).map(xyz => {
      val (x, y, z) = xyz
      (x, y, z, 0)
    })

    for(_ <- 0 until 6) {
      val potential = occupied union occupied.flatMap(getNeighbourCoords)
      occupied = potential.flatMap(p => {
        val neighbours = getNeighbourCoords(p)
        val numActiveNeighbours = neighbours.count(occupied.contains(_))
        val active = occupied.contains(p)
        if(active && (2 to 3).contains(numActiveNeighbours)) {
          Some(p)
        } else if(!active && numActiveNeighbours == 3) {
          Some(p)
        } else {
          None
        }
      })
    }

    occupied.size.toString
  }

  private def parseInput(input: String): Set[(Int, Int, Int)] = {
    HashSet() ++ input
      .split('\n')
      .zipWithIndex
      .flatMap(line => {
        val (chars, x) = line
        chars.zipWithIndex.filter(_._1 == '#').map(c => (x, c._2, 0))
      })
  }

  private def getNeighbourCoords(point: (Int, Int, Int)): Set[(Int, Int, Int)] = {
    val neighbours =
      for(x <- -1 to 1; y <- -1 to 1; z <- -1 to 1)
        yield point + (x, y, z)
    (HashSet() ++ neighbours) - point
  }

  private def getNeighbourCoords(point: (Int, Int, Int, Int)): Set[(Int, Int, Int, Int)] = {
    val neighbours =
      for(x <- -1 to 1; y <- -1 to 1; z <- -1 to 1; w <- -1 to 1)
        yield point + (x, y, z, w)
    (HashSet() ++ neighbours) - point
  }
}
