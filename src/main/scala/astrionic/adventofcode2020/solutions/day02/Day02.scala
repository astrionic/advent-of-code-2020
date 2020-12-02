package astrionic.adventofcode2020.solutions.day02

import astrionic.adventofcode2020.framework.AdventSolution

object Day02 extends AdventSolution {

  // writeSolution = true
  executePart = ExecutePart.One

  override def solvePart1(input: String): String = {
    val passwords = parseInput(input)
    val valid = passwords.filter(p => {
      val occurrences = p.password.count(_ == p.letter)
      occurrences >= p.min && occurrences <= p.max
    })
    valid.length.toString
  }

  override def solvePart2(input: String): String = {
    ???
  }

  private def parseInput(input: String): Array[Password] = {
    val lines = input.split('\n')
    lines.map(Password.fromString)
  }
}
