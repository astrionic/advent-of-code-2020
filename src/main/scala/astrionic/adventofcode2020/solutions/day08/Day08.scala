package astrionic.adventofcode2020.solutions.day08

import astrionic.adventofcode2020.framework.AdventSolution

object Day08 extends AdventSolution {

  override def solvePart1(input: String): String = {
    val programOption = parseProgram(input)
    if(programOption.isEmpty) return "Invalid program"
    val program = programOption.get

    var state = State(program, pc = 0, acc = 0)

    while(!state.willRepeatInstr) {
      state = executeNextInstr(state)
    }

    state.acc.toString
  }

  override def solvePart2(input: String): String = {
    val programOption = parseProgram(input)
    if(programOption.isEmpty) return "Invalid program"
    val originalProgram = programOption.get

    val potentialPrograms: List[List[Instr]] = createProgramPermutations(originalProgram)

    for(program <- potentialPrograms) {

      var state = State(program, pc = 0, acc = 0)

      while(!state.willTerminate && !state.willError && !state.willRepeatInstr) {
        state = executeNextInstr(state)
      }

      if(state.willTerminate) return state.acc.toString
    }
    
    "No solution found"
  }

  private case class State(
      program: List[Instr],
      pc: Int,
      acc: Int,
      executedLines: Set[Int] = Set[Int]()
  ) {
    def willRepeatInstr: Boolean =
      executedLines.contains(pc)

    def willTerminate: Boolean =
      pc == program.length

    def willError: Boolean =
      pc < 0 || pc > program.size
  }

  private trait Instr
  private case class Acc(value: Int) extends Instr
  private case class Jmp(value: Int) extends Instr
  private case class Nop(value: Int) extends Instr

  private def parseProgram(input: String): Option[List[Instr]] = {
    val program = input
      .split('\n')
      .map(line => {
        parseInstruction(line) match {
          case Some(instr) => instr
          case None        => return None
        }
      })
      .toList
    Some(program)
  }

  private def parseInstruction(input: String): Option[Instr] = {
    val parts = input.split(' ')
    if(parts.length != 2) return None

    val name = parts(0)
    val valueOption = parts(1).toIntOption

    (name, valueOption) match {
      case ("acc", Some(value)) => Some(Acc(value))
      case ("jmp", Some(value)) => Some(Jmp(value))
      case ("nop", Some(value)) => Some(Nop(value))
      case (_, _)               => None
    }
  }

  private def createProgramPermutations(program: List[Instr]): List[List[Instr]] = {
    program.zipWithIndex
      .filter(_._1 match {
        case Jmp(_) => true
        case Nop(_) => true
        case _      => false
      })
      .map(instrWithIndex => {
        val (instr, index) = instrWithIndex
        val newInstr = instr match {
          case Jmp(value) => Nop(value)
          case Nop(value) => Jmp(value)
        }
        program.updated(index, newInstr)
      })
  }

  private def executeNextInstr(state: State): State = {
    val pc = state.pc
    val acc = state.acc
    val newState = state.copy(executedLines = state.executedLines + pc)

    state.program(pc) match {
      case Acc(value) =>
        newState.copy(acc = acc + value, pc = pc + 1)
      case Jmp(value) =>
        newState.copy(pc = pc + value)
      case Nop(_) =>
        newState.copy(pc = pc + 1)
    }
  }
}
