package astrionic.adventofcode2020.solutions.day16

import astrionic.adventofcode2020.framework.AdventSolution

object Day16 extends AdventSolution {

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
    val in: PuzzleInput = parseInput(input)
    val allRanges = in.rules.flatMap(_.ranges)

    val validTickets = in.nearbyTickets
      .filter(ticket => ticket.forall(field => allRanges.count(_.contains(field)) > 0))

    val rulesWithPotentialPositions = in.rules.map(rule => {
      val potentialPositions = validTickets
        .map(ticket => {
          ticket.zipWithIndex.filter(field => rule.ranges.count(_.contains(field._1)) > 0).map(_._2).toSet
        })
        .reduce(_ intersect _)
      (rule.name, potentialPositions)
    })

    val (a, d) = rulesWithPotentialPositions.partition(_._2.size > 1)
    var ambiguous = a.toSet
    var definitive = d.toSet

    while(ambiguous.nonEmpty) {
      ambiguous = ambiguous.map(a => {
        (a._1, a._2 diff definitive.flatMap(_._2))
      })
      val (newAmbiguous, newDefinitive) = ambiguous.partition(_._2.size > 1)
      definitive ++= newDefinitive
      ambiguous = newAmbiguous
    }

    val departureFieldIndices = definitive.filter(_._1.startsWith("departure")).flatMap(_._2)

    departureFieldIndices
      .map(
        in.myTicket(_).toLong
      )
      .product
      .toString
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
