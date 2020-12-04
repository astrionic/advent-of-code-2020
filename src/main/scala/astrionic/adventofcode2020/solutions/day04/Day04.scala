package astrionic.adventofcode2020.solutions.day04

import astrionic.adventofcode2020.framework.AdventSolution

object Day04 extends AdventSolution {

  override def solvePart1(input: String): String = {
    val passports = input.split("\n\n")

    passports.count(containsAll(_, requiredFields)).toString
  }

  override def solvePart2(input: String): String = {
    val passports = input.split("\n\n")
    val passportsWithRequiredFields = passports.filter(containsAll(_, requiredFields))

    val parsedPassports: Array[Array[PassportField]] = parsePassports(passportsWithRequiredFields)

    val validPassports = parsedPassports
      .filter(p => {
        p.map(field => {
          val fOption = Passport.fieldRules.get(field.name)
          fOption match {
            case Some(f) => f(field.value)
            case None    => false
          }
        }).count(_ == false) == 0
      })

    validPassports.length.toString
  }

  private val requiredFields = List(
    "byr",
    "iyr",
    "eyr",
    "hgt",
    "hcl",
    "ecl",
    "pid"
  )

  private def containsAll(s: String, patterns: List[String]): Boolean = {
    for(pattern <- patterns) {
      if(!s.contains(pattern)) {
        return false;
      }
    }
    true
  }

  private def parsePassports(passports: Array[String]): Array[Array[PassportField]] = {
    passports.map(passport => {
      val passportFields = passport
        .split("\\s")
        .map(field => {
          val pair = field.split(":")
          PassportField(pair(0), pair(1))
        })
      passportFields.filter(field => requiredFields.contains(field.name))
    })
  }
}
