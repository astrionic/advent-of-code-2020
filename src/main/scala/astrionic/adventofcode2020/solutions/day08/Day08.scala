package astrionic.adventofcode2020.solutions.day08

import astrionic.adventofcode2020.framework.AdventSolution

object Day08 extends AdventSolution {

  override def solvePart1(input: String): String = {
    val programOption = parseProgram(input)
    if(programOption.isEmpty) return "Invalid program"
    val program = programOption.get

    var state = ComputerState(program, pc = 0, acc = 0)

    while(!state.willRepeatInstr) {
      state = state.executeNextInstr()
    }

    state.acc.toString
  }

  override def solvePart2(input: String): String = {
    val programOption = parseProgram(input)
    if(programOption.isEmpty) return "Invalid program"
    val originalProgram = programOption.get
    val potentialPrograms: List[List[Instruction]] = createProgramPermutations(originalProgram)

    for(program <- potentialPrograms) {

      var state = ComputerState(program, pc = 0, acc = 0)
      while(!state.willTerminate && !state.willError && !state.willRepeatInstr) {
        state = state.executeNextInstr()
      }
      if(state.willTerminate) return state.acc.toString
    }

    "No solution found"
  }

  private def parseProgram(input: String): Option[List[Instruction]] = {
    val program = input
      .split('\n')
      .map(line => {
        Instruction.fromString(line) match {
          case Some(instr) => instr
          case None        => return None
        }
      })
      .toList
    Some(program)
  }

  private def createProgramPermutations(program: List[Instruction]): List[List[Instruction]] = {
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
}
