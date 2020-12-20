package astrionic.adventofcode2020.solutions.day18

import astrionic.adventofcode2020.framework.AdventSolution

object Day18 extends AdventSolution {

  // writeSolution = true
  executePart = ExecutePart.One

  override def solvePart1(input: String): String = {
    val tokens = parseInput(input)
    tokens.map(eval).sum.toString
  }

  override def solvePart2(input: String): String = {
    ???
  }

  private def eval(tokens: List[Token]): Long = {
    if(tokens.contains(ParenOpen) && tokens.contains(ParenClose)) {
      val openIndex = tokens.indexOf(ParenOpen)
      var openParens = 1
      var i = openIndex + 1
      while(openParens > 0) {
        tokens(i) match {
          case ParenOpen  => openParens += 1
          case ParenClose => openParens -= 1
          case _          => // do nothing
        }
        i += 1
      }

      val before = tokens.slice(0, openIndex)
      val current = List(Number(eval(tokens.slice(openIndex + 1, i - 1))))
      val after = tokens.slice(i, tokens.length)
      return eval(before ++ current ++ after)
    }
    tokens match {
      case Number(n) :: Nil                        => n
      case Number(a) :: Times :: Number(b) :: tail => eval(Number(a * b) :: tail)
      case Number(a) :: Plus :: Number(b) :: tail  => eval(Number(a + b) :: tail)
      case x                                       => println(x); throw new Exception()
    }
  }

  private trait Token

  private case class Number(value: Long) extends Token

  private trait Operator extends Token

  private object Plus extends Operator

  private object Times extends Operator

  private object ParenOpen extends Token

  private object ParenClose extends Token

  private def parseInput(input: String): List[List[Token]] =
    input.split('\n').map(parseExpression).toList

  private def parseExpression(exp: String): List[Token] =
    exp.split(' ').flatMap(parseToken).toList

  private val plus = "(\\+)".r
  private val times = "(\\*)".r
  private val number = "(\\d+)".r
  private val parenOpenPrefix = "(\\()(.*)".r
  private val parenCloseSuffix = "(.*)(\\))".r

  private def parseToken(token: String): List[Token] = token match {
    case plus(_)                => List(Plus)
    case times(_)               => List(Times)
    case number(n)              => List(Number(n.toLong))
    case parenOpenPrefix(_, r)  => ParenOpen :: parseToken(r)
    case parenCloseSuffix(r, _) => parseToken(r) :+ ParenClose
    case ""                     => List()
    case _                      => throw new Exception
  }
}
