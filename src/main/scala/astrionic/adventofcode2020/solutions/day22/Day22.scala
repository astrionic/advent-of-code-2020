package astrionic.adventofcode2020.solutions.day22

import astrionic.adventofcode2020.framework.AdventSolution
import astrionic.adventofcode2020.solutions.day22.Day22.Player.Player

object Day22 extends AdventSolution {

  writeSolution = true
  executePart = ExecutePart.Two

  override def solvePart1(input: String): String = {
    var (deck1, deck2) = parseInput(input)

    while(deck1.nonEmpty && deck2.nonEmpty) {
      if(deck1.head > deck2.head) {
        deck1 = deck1.tail ++ List(deck1.head, deck2.head)
        deck2 = deck2.tail
      } else {
        deck2 = deck2.tail ++ List(deck2.head, deck1.head)
        deck1 = deck1.tail
      }
    }

    val winningDeck = if(deck1.nonEmpty) deck1 else deck2
    calculateWinningScore(winningDeck).toString
  }

  override def solvePart2(input: String): String = {
    val (deck1, deck2) = parseInput(input)
    val winningDeck = recursiveCombat(deck1, deck2).winningDeck
    calculateWinningScore(winningDeck).toString
  }

  private def parseInput(input: String): (List[Int], List[Int]) = {
    val regex = """Player 1:\n([\d\n]+)\nPlayer 2:\n([\d\n]+)""".r
    input match {
      case regex(p1, p2) => (p1.split('\n').map(_.toInt).toList, p2.split('\n').map(_.toInt).toList)
      case _             => throw new Exception
    }
  }

  private def calculateWinningScore(winningDeck: List[Int]): Int = {
    winningDeck
      .zip((1 to winningDeck.length).reverse)
      .map(x => x._1 * x._2)
      .sum
  }

  private[day22] object Player extends Enumeration {
    type Player = Value
    val One, Two = Value
  }

  private case class GameResult(winner: Player, winningDeck: List[Int])

  private def recursiveCombat(deck1: List[Int], deck2: List[Int]): GameResult = {
    var previousStates: Set[(List[Int], List[Int])] = Set()
    var (d1, d2): (List[Int], List[Int]) = (deck1, deck2)

    while(d1.nonEmpty && d2.nonEmpty) {
      // If this state has happened before player 1 wins immediately
      if(previousStates.contains((d1, d2))) {
        return GameResult(Player.One, d1)
      }

      // Save state
      val state = (d1, d2)
      previousStates += state

      // Draw cards
      val (c1, c2) = (d1.head, d2.head)
      d1 = d1.tail
      d2 = d2.tail

      // Determine winner
      val roundWinner = if(d1.length >= c1 && d2.length >= c2) {
        // Shahrazad
        recursiveCombat(d1.slice(0, c1), d2.slice(0, c2)).winner
      } else {
        if(c1 > c2) Player.One else Player.Two
      }

      // Put cards on the bottom of the winner's deck
      roundWinner match {
        case Player.One => d1 :++= List(c1, c2)
        case Player.Two => d2 :++= List(c2, c1)
      }
    }

    if(d1.nonEmpty) {
      GameResult(Player.One, d1)
    } else {
      GameResult(Player.Two, d2)
    }
  }
}
