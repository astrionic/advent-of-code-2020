package astrionic.adventofcode2020.solutions.day08

import astrionic.adventofcode2020.framework.AdventSolution

object Day08 extends AdventSolution {

  // writeSolution = true
  executePart = ExecutePart.One

  override def solvePart1(input: String): String = {
    val program: Array[Instruction] = parseInput(input)

    var acc = 0
    var pc = 0
    var previouslyExecuted = Set[Int]()

    while(!previouslyExecuted.contains(pc)) {
      previouslyExecuted += pc

      program(pc) match {
        case Instruction("acc", value) =>
          acc += value
          pc += 1
        case Instruction("jmp", value) =>
          pc += value
        case Instruction("nop", _) =>
          pc += 1
        case Instruction(_, _) => throw new Exception("Invalid operation")
      }
    }

    acc.toString
  }

  override def solvePart2(input: String): String = {
    ???
  }

  private case class Instruction(name: String, value: Int)

  private def parseInput(input: String): Array[Instruction] = {
    input
      .split('\n')
      .map(line => {
        val parts = line.split(' ')
        val name = parts(0)
        val value = parts(1).toInt
        Instruction(name, value)
      })
  }
}
