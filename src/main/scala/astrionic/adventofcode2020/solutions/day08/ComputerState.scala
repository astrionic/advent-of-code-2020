package astrionic.adventofcode2020.solutions.day08

/**
 * @param program
 *   Program
 * @param pc
 *   Program counter
 * @param acc
 *   Accumulator
 * @param executedLines
 *   Set containing the line number of each line that has previously been executed
 */
private[day08] case class ComputerState(
    program: List[Instruction],
    pc: Int,
    acc: Int,
    executedLines: Set[Int] = Set[Int]()
) {

  /**
   * @return
   *   True if the next instruction/line has previously been executed
   */
  def willRepeatInstr: Boolean =
    executedLines.contains(pc)

  /**
   * @return
   *   True if the program will terminate with the next instruction
   */
  def willTerminate: Boolean =
    pc == program.length

  /**
   * @return
   *   True if the program will error with the next instruction
   */
  def willError: Boolean =
    pc < 0 || pc > program.size

  /**
   * @return
   *   Executes the next instruction in the program and returns the resulting state
   */
  def executeNextInstr(): ComputerState = {
    val newState = copy(executedLines = executedLines + pc)

    program(pc) match {
      case Acc(value) =>
        newState.copy(acc = acc + value, pc = pc + 1)
      case Jmp(value) =>
        newState.copy(pc = pc + value)
      case Nop(_) =>
        newState.copy(pc = pc + 1)
    }
  }
}
