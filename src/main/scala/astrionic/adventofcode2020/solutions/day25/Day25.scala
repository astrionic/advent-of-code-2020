package astrionic.adventofcode2020.solutions.day25

import astrionic.adventofcode2020.framework.AdventSolution

object Day25 extends AdventSolution {

  // writeSolution = true
  executePart = ExecutePart.One

  override def solvePart1(input: String): String = {
    val (pub0, pub1) = parseInput(input)
    val (loop0, loop1) = (findLoopSize(pub0), findLoopSize(pub1))
    encode(pub0, loop1.get).toString
  }

  override def solvePart2(input: String): String = {
    ???
  }

  private def parseInput(input: String): (Long, Long) = {
    val keyPattern = """^(\d+)\n(\d+)$""".r
    input match {
      case keyPattern(key0, key1) => (key0.toLong, key1.toLong)
      case _                      => throw new Exception("Invalid input")
    }
  }

  private def findLoopSize(pubKey: Long, maxIter: Long = 1_000_000_000): Option[Long] = {
    val subjectNum = 7L
    var n = 1L
    for(loopSize <- 1L to maxIter) {
      n = (n * subjectNum) % 20201227
      if(n == pubKey) return Some(loopSize)
    }
    None
  }

  private def encode(subjectNum: Long, loopSize: Long): Long = {
    (0L until loopSize).foldLeft(1L)((n, _) => (n * subjectNum) % 20201227)
  }
}
