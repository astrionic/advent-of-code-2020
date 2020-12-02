package astrionic.adventofcode2020.solutions.day02

import astrionic.adventofcode2020.framework.AdventSolution

object Day02 extends AdventSolution {

  override def solvePart1(input: String): String = {
    val passwords = parseInput(input)
    val valid = passwords.filter(p => {
      val occurrences = p.password.count(_ == p.letter)
      occurrences >= p.min && occurrences <= p.max
    })
    valid.length.toString
  }

  override def solvePart2(input: String): String = {
    val passwords = parseInput(input)
    val valid = passwords.filter(p => {
      (p.password(p.min - 1) == p.letter && p.password(p.max - 1) != p.letter) ||
        (p.password(p.min - 1) != p.letter && p.password(p.max - 1) == p.letter)
    })
    valid.length.toString
  }

  private def parseInput(input: String): Array[Password] = {
    val lines = input.split('\n')
    lines.map(Password.fromString)
  }
}
