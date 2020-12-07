package astrionic.adventofcode2020.solutions.day07

import astrionic.adventofcode2020.framework.AdventSolution

object Day07 extends AdventSolution {

  override def solvePart1(input: String): String = {
    val rules: Array[Rule] = parseInput(input)

    var goldenContainers: Set[String] = rules
      .filter(_.canContain("shiny gold"))
      .map(_.colour)
      .toSet
    var previousSize = 0

    while(goldenContainers.size > previousSize) {
      previousSize = goldenContainers.size
      val updatedContainers = rules
        .filter(_.canContainOneOf(goldenContainers))
        .map(_.colour)
        .toSet
      goldenContainers ++= updatedContainers
    }
    goldenContainers.size.toString
  }

  override def solvePart2(input: String): String = {
    val rules: Array[Rule] = parseInput(input)
    val numBags = calculateTotalNumBags("shiny gold", rules)
    (numBags - 1).toString // Remove the shiny gold bag itself
  }

  private case class Rule(colour: String, contents: Array[Content]) {
    def canContain(someColour: String): Boolean = {
      contents.map(_.colour).contains(someColour)
    }

    def canContainOneOf(colours: Set[String]): Boolean = {
      colours.toList.map(this.canContain).reduce(_ || _)
    }
  }
  
  private case class Content(amount: Int, colour: String)

  private def parseInput(input: String): Array[Rule] = {
    val lines = input
      .replaceAll(" bags?", "")
      .split(".\n")

    lines
      .map(_.split(" contain "))
      .map(r => {
        val colour = r(0)
        val contents = r(1) match {
          case "no other" => Array[Content]()
          case other =>
            other
              .split(", ")
              .map(c => {
                val firstSpace = c.indexOf(' ')
                val amount = c.substring(0, firstSpace).toInt
                val colour = c.substring(firstSpace + 1)
                Content(amount.toInt, colour)
              })
        }
        Rule(colour, contents)
      })
  }

  private def calculateTotalNumBags(colour: String, rules: Array[Rule]): Int = {
    val ruleOption: Option[Rule] = rules.find(_.colour == colour)
    ruleOption match {
      case Some(rule) =>
        1 + rule.contents
          .map(content => content.amount * calculateTotalNumBags(content.colour, rules))
          .sum
      case None => throw new Exception("No matching rule found")
    }
  }
}
