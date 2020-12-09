package astrionic.adventofcode2020.solutions.day09

import astrionic.adventofcode2020.framework.AdventSolution

object Day09 extends AdventSolution {

  writeSolution = true
  executePart = ExecutePart.One

  override def solvePart1(input: String): String = {
    val ns = parseInput(input)
    val preambleLength = 25

    val nsWithTwoSums = ns.zipWithIndex
      .slice(preambleLength, ns.length)
      .map(y => {
        val (n, i) = y
        val previous = ns.slice(i - preambleLength, i)
        (twoSum(previous, n), n)
      })

    nsWithTwoSums.filter(_._1.isEmpty).head.toString
  }

  override def solvePart2(input: String): String = {
    ???
  }

  private def parseInput(input: String): List[Long] = input.split('\n').map(_.toLong).toList

  def twoSum(nums: List[Long], target: Long): Option[(Long, Long)] = {
    val set = nums.toSet
    for(n <- nums) {
      val complement = target - n
      if(set.contains(complement)) {
        return Some(n, complement)
      }
    }
    None
  }
}
