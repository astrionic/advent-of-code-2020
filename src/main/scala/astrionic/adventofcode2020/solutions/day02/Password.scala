package astrionic.adventofcode2020.solutions.day02

case class Password(
    min: Int,
    max: Int,
    letter: Char,
    password: String
)

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
