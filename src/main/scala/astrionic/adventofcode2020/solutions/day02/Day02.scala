package astrionic.adventofcode2020.solutions.day02

import astrionic.adventofcode2020.framework.AdventSolution

object Day02 extends AdventSolution {

  override def solvePart1(input: String): String = {
    val passwords = parseInput(input)
    val validPasswords = passwords.filter(_.isValidPart1)

    validPasswords.length.toString
  }

  override def solvePart2(input: String): String = {
    val passwords = parseInput(input)
    val validPasswords = passwords.filter(_.isValidPart2)

    validPasswords.length.toString
  }

  private def parseInput(input: String): Array[Password] =
    input.split('\n').map(Password.fromString)
}
