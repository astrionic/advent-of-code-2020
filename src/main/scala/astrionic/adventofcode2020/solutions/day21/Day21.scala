package astrionic.adventofcode2020.solutions.day21

import astrionic.adventofcode2020.framework.AdventSolution

//noinspection DuplicatedCode
object Day21 extends AdventSolution {

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
    val food = parseInput(input)

    val allergens = food.flatMap(_.allergens).toSet
    val allergenWithIngredients: Map[String, Set[String]] = allergens
      .map(allergen => {
        val potentialIngredients = food
          .filter(_.allergens.contains(allergen))
          .map(_.ingredients.toSet)
          .reduce(_ intersect _)
        (allergen, potentialIngredients)
      })
      .toMap

    var definitive = allergenWithIngredients
      .filter(_._2.size == 1)
      .map(awi => (awi._1, awi._2.head))

    var ambiguous = allergenWithIngredients
      .filter(_._2.size > 1)
    while(ambiguous.nonEmpty) {
      ambiguous = ambiguous.map(awi => {
        val (a, in) = awi
        (a, in diff definitive.values.toSet)
      })
      definitive ++= ambiguous.filter(_._2.size == 1).map(awi => (awi._1, awi._2.head))
      ambiguous = ambiguous.filter(_._2.size > 1)
    }

    definitive.toList.sortBy(_._1).map(_._2).mkString(",")
  }

  private case class Food(ingredients: Set[String], allergens: Set[String])

  private def parseInput(input: String): List[Food] = {
    val lines = input.split('\n')
    val foodRegex = "([a-z ]+) \\(contains ([a-z ,]+)\\)".r
    lines
      .map({
        case foodRegex(ingredients, allergens) =>
          Food(ingredients.split(' ').toSet, allergens.split(", ").toSet)
        case _ => throw new Exception
      })
      .toList
  }
}
