package astrionic.adventofcode2020.solutions.day21

import astrionic.adventofcode2020.framework.AdventSolution

object Day21 extends AdventSolution {

  // writeSolution = true
  executePart = ExecutePart.One

  override def solvePart1(input: String): String = {
    val food = parseInput(input)

    val allergens = food.flatMap(_.allergens).toSet
    val canContainAllergens: Set[String] = allergens.flatMap(a => {
      food
        .filter(_.allergens.contains(a))
        .map(_.ingredients.toSet)
        .reduce(_ intersect _)
    })
    val ingredients: Set[String] = food.flatMap(_.ingredients).toSet
    val cantContainAllergens = ingredients diff canContainAllergens

    food.flatMap(_.ingredients).count(cantContainAllergens.contains).toString
  }

  override def solvePart2(input: String): String = {
    ???
  }

  private case class Food(ingredients: List[String], allergens: List[String])

  private def parseInput(input: String): List[Food] = {
    val lines = input.split('\n')
    val foodRegex = "([a-z ]+) \\(contains ([a-z ,]+)\\)".r
    lines
      .map({
        case foodRegex(ingredients, allergens) =>
          Food(ingredients.split(' ').toList, allergens.split(", ").toList)
        case _ => throw new Exception
      })
      .toList
  }
}
