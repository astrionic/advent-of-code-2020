package astrionic.adventofcode2020.solutions.day01

import astrionic.adventofcode2020.framework.AdventSolution

import scala.collection.immutable.HashSet
import scala.collection.mutable

object Day01 extends AdventSolution {

  // writeSolution = true
  executePart = ExecutePart.One

  override def solvePart1(input: String): String = {
    val nums = input.split("\n").toList.map(_.toInt)
    val target = 2020

    twoSum(nums, target) match {
      case Some((num0, num1)) => (num0 * num1).toString
      case None               => "No solution found"
    }
  }

  override def solvePart2(input: String): String = {
    ???
  }

  def twoSum(nums: List[Int], target: Int): Option[(Int, Int)] = {
    val set = nums.toSet
    nums.filter(n => set.contains(target - n)) match {
      case x :: _ => Some(x, target - x)
      case Nil    => None
    }
  }


}
