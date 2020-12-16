package astrionic.adventofcode2020.solutions.day16

import astrionic.adventofcode2020.framework.AdventSolution

object Day16 extends AdventSolution {

  // writeSolution = true
  executePart = ExecutePart.One

  override def solvePart1(input: String): String = {
    val in: PuzzleInput = parseInput(input)
    val ranges: Seq[Range] = in.rules.flatMap(_.ranges)
    val values: Seq[Int] = in.nearbyTickets.flatten
    values
      .filter(x => ranges.count(_.contains(x)) == 0)
      .sum
      .toString
  }

  override def solvePart2(input: String): String = {
    ???
  }

  private case class PuzzleInput(
      rules: List[Rule],
      myTicket: List[Int],
      nearbyTickets: List[List[Int]]
  )
  private case class Rule(
      name: String,
      ranges: List[Range]
  )

  private def parseInput(input: String): PuzzleInput = {
    val Array(rulesInput, myTicketInput, nearbyTicketsInput) = input.split("\n\n")

    // Parse rules
    val rules = rulesInput
      .split('\n')
      .map(line => {
        val Array(name, rangesString) = line.split(": ")
        val ranges = rangesString
          .split(" or ")
          .map(r => {
            val x = r.split('-')
            x(0).toInt to x(1).toInt
          })
          .toList

        Rule(name, ranges)
      })
      .toList

    // Parse my ticket
    val myTicket = myTicketInput.split('\n')(1).split(',').map(_.toInt).toList

    // Parse nearby tickets
    val nearbyTickets = nearbyTicketsInput
      .split('\n')
      .tail
      .map(_.split(',').map(_.toInt).toList)
      .toList

    PuzzleInput(rules, myTicket, nearbyTickets)
  }
}
