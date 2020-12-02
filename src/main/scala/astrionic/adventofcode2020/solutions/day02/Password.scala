package astrionic.adventofcode2020.solutions.day02

case class Password(
    min: Int,
    max: Int,
    letter: Char,
    password: String
) {
  def isValidPart1: Boolean = {
    val occurrences = password.count(_ == letter)
    occurrences >= min && occurrences <= max
  }

  def isValidPart2: Boolean = {
    (password(min - 1) == letter && password(max - 1) != letter) ||
    (password(min - 1) != letter && password(max - 1) == letter)
  }
}

object Password {
  def fromString(s: String): Password = {
    val a = s.split(": ")
    val password = a(1)

    val b = a(0).split(" ")
    val letter = b(1).toCharArray.head

    val c = b(0).split("-")
    val min = c(0).toInt
    val max = c(1).toInt

    Password(min, max, letter, password)
  }
}
