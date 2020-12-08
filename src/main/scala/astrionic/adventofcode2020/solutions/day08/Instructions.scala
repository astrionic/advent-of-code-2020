package astrionic.adventofcode2020.solutions.day08

private[day08] trait Instruction

private[day08] object Instruction {
  def fromString(s: String): Option[Instruction] = {
    val parts = s.split(' ')
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
}

private[day08] case class Acc(value: Int) extends Instruction

private[day08] case class Jmp(value: Int) extends Instruction

private[day08] case class Nop(value: Int) extends Instruction
