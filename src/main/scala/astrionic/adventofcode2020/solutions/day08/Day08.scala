package astrionic.adventofcode2020.solutions.day08

import astrionic.adventofcode2020.framework.AdventSolution

//noinspection DuplicatedCode
object Day08 extends AdventSolution {

  override def solvePart1(input: String): String = {
    val program: List[Instruction] = parseInput(input)

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
    val program: List[Instruction] = parseInput(input)
    val programs: List[List[Instruction]] = createProgramPermutations(program)

    for(p <- programs) {
      var terminated = false
      var error = false
      var acc = 0
      var pc = 0
      var previouslyExecuted = Set[Int]()

      while(!terminated && !error && !previouslyExecuted.contains(pc)) {
        if(pc == p.length) {
          terminated = true
        } else if(pc > p.length || pc < 0) {
          error = true
        } else {
          previouslyExecuted += pc

          p(pc) match {
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
      }

      if(terminated) return acc.toString
    }
    return "No solution found"
  }

  private case class Instruction(name: String, value: Int)

  private def parseInput(input: String): List[Instruction] = {
    input
      .split('\n')
      .map(line => {
        val parts = line.split(' ')
        val name = parts(0)
        val value = parts(1).toInt
        Instruction(name, value)
      })
      .toList
  }

  private def createProgramPermutations(program: List[Instruction]) = {
    program.zipWithIndex
      .filter(i => i._1.name == "jmp" || i._1.name == "nop")
      .map(i => {
        val newInstr = i._1 match {
          case Instruction("jmp", value) => Instruction("nop", value)
          case Instruction("nop", value) => Instruction("jmp", value)
        }
        program.updated(i._2, newInstr)
      })
  }
}
