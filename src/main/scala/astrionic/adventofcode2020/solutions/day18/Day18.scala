package astrionic.adventofcode2020.solutions.day18

import astrionic.adventofcode2020.framework.AdventSolution

import scala.collection.mutable

object Day18 extends AdventSolution {

  override def solvePart1(input: String): String = {
    val tokens = parseInput(input)
    tokens.map(eval).sum.toString
  }

  override def solvePart2(input: String): String = {
    implicit val part2: Boolean = true
    val tokens = parseInput(input)
    tokens.map(eval).sum.toString
  }

  private def eval(tokens: List[Token])(implicit part2: Boolean = false): Long = {
    val nums = new mutable.Stack[Long]()
    val ops = new mutable.Stack[Token]()

    for(token <- tokens) token match {
      case Number(n) => nums.push(n)
      case ParenOpen => ops.push(ParenOpen)
      case ParenClose =>
        while(ops.head != ParenOpen) {
          ops.pop() match {
            case op: Operator => nums.push(calculate(nums.pop(), op, nums.pop()))
            case _            => throw new Exception()
          }
        }
        ops.pop()
      case op: Operator =>
        while(
          ops.nonEmpty &&
          ops.head.isInstanceOf[Operator] &&
          ops.head.asInstanceOf[Operator].precedence >= op.precedence
        ) {
          nums.push(calculate(nums.pop(), ops.pop().asInstanceOf[Operator], nums.pop()))
        }
        ops.push(op)
    }

    while(ops.nonEmpty) {
      nums.push(calculate(nums.pop(), ops.pop().asInstanceOf[Operator], nums.pop()))
    }
    
    nums.pop()
  }

  private def calculate(a: Long, op: Operator, b: Long): Long = op match {
    case Plus  => a + b
    case Times => a * b
  }

  private trait Token

  private case class Number(value: Long) extends Token

  private trait Operator extends Token {
    def precedence(implicit part2: Boolean): Int
  }

  private object Plus extends Operator {
    def precedence(implicit part2: Boolean = false): Int = if(part2) 1 else 0
  }

  private object Times extends Operator {
    def precedence(implicit part2: Boolean = false): Int = 0
  }

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
