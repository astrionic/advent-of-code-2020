package astrionic.adventofcode2020.solutions.day04

import scala.util.matching.Regex

private[day04] object Passport {
  def fieldRules: Map[String, String => Boolean] = Map(
    "byr" -> (yearIsValid(_, 1920, 2002)),
    "iyr" -> (yearIsValid(_, 2010, 2020)),
    "eyr" -> (yearIsValid(_, 2020, 2030)),
    "hgt" -> heightIsValid,
    "hcl" -> hairColourIsValid,
    "ecl" -> eyeColourIsValid,
    "pid" -> passportIdIsValid
  )

  private def heightIsValid(height: String): Boolean = {
    val pattern: Regex = "\\d+(in|cm)".r
    if(!pattern.matches(height)) return false

    val n = height.slice(0, height.length - 2).toIntOption
    val unit = height.slice(height.length - 2, height.length)

    (n, unit) match {
      case (Some(n), "cm") => n >= 150 && n <= 193
      case (Some(n), "in") => n >= 59 && n <= 76
      case _               => false
    }
  }

  private def hairColourIsValid(colour: String): Boolean = {
    val pattern: Regex = "#[0-9a-fA-F]{6}".r
    pattern.matches(colour)
  }

  private def eyeColourIsValid(colour: String): Boolean = {
    val validEyeColours = List(
      "amb",
      "blu",
      "brn",
      "gry",
      "grn",
      "hzl",
      "oth"
    )

    validEyeColours.count(_ == colour) == 1
  }

  private def passportIdIsValid(id: String): Boolean =
    "[0-9]{9}".r.matches(id)

  private def yearIsValid(yearString: String, atLeast: Int, atMost: Int): Boolean = {
    yearString.length == 4 && (yearString.toIntOption match {
      case Some(year) => year >= atLeast && year <= atMost
      case None       => false
    })
  }
}
