package astrionic.adventofcode2020.solutions.day19

import astrionic.adventofcode2020.framework.AdventSolution

import scala.collection.mutable

object Day19 extends AdventSolution {

  writeSolution = true
  executePart = ExecutePart.Two

  override def solvePart1(input: String): String = {
    val (rules, messages) = parseInput(input)
    val ruleCache = new RuleCache(rules)
    val regex = ruleCache.getRuleRegex(0).r
    messages.count(regex.matches(_)).toString
  }

  override def solvePart2(input: String): String = {
    val (rules, messages) = parseInput(input)
    val ruleCache = new RuleCache(rules)

    // Fix rule 8 (dirty hack)
    val regex8 = ruleCache.getRuleRegex(8)
    ruleCache.cache.update(8, s"$regex8+")

    // Fix rule 11 (much dirtier hack)
    val regex31 = ruleCache.getRuleRegex(31)
    val regex42 = ruleCache.getRuleRegex(42)

    val longestMsgLength = messages.map(_.length).max
    val regex11combinations: Seq[String] = (1 to (longestMsgLength / 2)).map(n => {
      s"($regex42){$n}($regex31){$n}"
    })

    val regex11 = s"(${regex11combinations.mkString("|")})"
    ruleCache.cache.update(11, regex11)

    // Check rule 0
    val regex0 = ruleCache.getRuleRegex(0).r
    messages.count(regex0.matches(_)).toString
  }

  private trait Rule

  private case class BaseRule(char: Char) extends Rule
  private case class ComposedRule(rules: List[List[Int]]) extends Rule

  private def parseInput(input: String): (Map[Int, Rule], List[String]) = {
    val Array(rulesPart, messagePart) = input.split("\n\n")

    val rulePattern = "(\\d+): (\"[ab]\"|[\\s\\d|]+)".r
    val rules = rulesPart
      .split('\n')
      .flatMap(_ match {
        case rulePattern(id, rule) => Some((id.toInt, parseRule(rule)))
        case _                     => None
      })
      .toMap

    val messages = messagePart.split('\n').toList

    (rules, messages)
  }

  private def parseRule(rule: String): Rule = {
    val baseRulePattern = "\"([ab])\"".r
    rule match {
      case baseRulePattern(c) => BaseRule(c.head)
      case rule =>
        ComposedRule(
          rule
            .split(" \\| ")
            .map(_.split(' ').map(_.toInt).toList)
            .toList
        )
    }
  }

  private class RuleCache(rules: Map[Int, Rule]) {
    val cache: mutable.Map[Int, String] = mutable.Map[Int, String]()

    def getRuleRegex(id: Int): String = {
      if(!cache.contains(id)) {
        val regex = rules(id) match {
          case BaseRule(char) => char.toString
          case ComposedRule(rules) =>
            val r = rules.map(_.map(getRuleRegex).mkString).mkString("|")
            s"($r)"

        }
        cache.addOne(id -> regex)
      }
      cache(id)
    }
  }
}
