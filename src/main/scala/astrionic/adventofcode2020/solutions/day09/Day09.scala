package astrionic.adventofcode2020.solutions.day09

import astrionic.adventofcode2020.framework.AdventSolution

import scala.collection.immutable.Queue

//noinspection DuplicatedCode
object Day09 extends AdventSolution {

  override def solvePart1(input: String): String = {
    val nums = parseInput(input)
    val preambleLength = 25

    val numsWithIdx = nums.zipWithIndex
      .slice(preambleLength, nums.length)
      .map(numWithIdx => {
        val (num, idx) = numWithIdx
        val previous = nums.slice(idx - preambleLength, idx)
        (twoSum(previous, num), num)
      })

    numsWithIdx.filter(_._1.isEmpty).head._2.toString
  }

  override def solvePart2(input: String): String = {
    val target = 144381670 // Solution from part 1
    val nums: List[Long] = parseInput(input).filter(_ != target)

    var q = Queue[Long]()
    for(num: Long <- nums) {
      while(q.sum > target) {
        val (_, tail) = q.dequeue
        q = tail
      }
      if(q.sum == target) return (q.min + q.max).toString
      q = q.enqueue(num)
    }
    "No solution found"
  }

  private def parseInput(input: String): List[Long] = input.split('\n').map(_.toLong).toList

  private def twoSum(nums: List[Long], target: Long): Option[(Long, Long)] = {
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
