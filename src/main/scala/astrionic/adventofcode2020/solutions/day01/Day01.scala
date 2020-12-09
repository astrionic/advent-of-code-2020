package astrionic.adventofcode2020.solutions.day01

import astrionic.adventofcode2020.framework.AdventSolution

//noinspection DuplicatedCode
object Day01 extends AdventSolution {

  override def solvePart1(input: String): String = {
    val nums = input.split("\n").toList.map(_.toInt)
    val target = 2020

    twoSum(nums, target) match {
      case Some((n0, n1)) => (n0 * n1).toString
      case None           => "No solution found"
    }
  }

  override def solvePart2(input: String): String = {
    val nums = input.split("\n").toList.map(_.toInt)
    val target = 2020

    threeSum(nums, target) match {
      case Some((n0, n1, n2)) => (n0 * n1 * n2).toString
      case None               => "No solution found"
    }
  }

  def twoSum(nums: List[Int], target: Int): Option[(Int, Int)] = {
    val set = nums.toSet
    for(n <- nums) {
      val complement = target - n
      if(set.contains(complement)) {
        return Some(n, complement)
      }
    }
    None
  }

  def threeSum(nums: List[Int], target: Int): Option[(Int, Int, Int)] = {
    for(n0 <- nums) {
      val complement = target - n0
      twoSum(nums, complement) match {
        case Some((n1, n2)) => return Some(n0, n1, n2)
        case None           =>
      }
    }
    None
  }
}
