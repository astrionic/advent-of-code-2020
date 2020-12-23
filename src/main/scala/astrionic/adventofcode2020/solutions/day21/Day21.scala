package astrionic.adventofcode2020.solutions.day21

import astrionic.adventofcode2020.framework.AdventSolution

object Day21 extends AdventSolution {

  override def solvePart1(input: String): String = {
    val food = parseInput(input)

    val ingredientsWithAllergenRisk: Set[String] = mapIngredientsToAllergens(food)
      .flatMap(_.potentialIngredients)
      .toSet

    val ingredients: Set[String] = food.flatMap(_.ingredients).toSet
    val cantContainAllergens = ingredients diff ingredientsWithAllergenRisk

    food.flatMap(_.ingredients).count(cantContainAllergens.contains).toString
  }

  override def solvePart2(input: String): String = {
    val food = parseInput(input)

    val allergens = mapIngredientsToAllergens(food)

    var definitive = allergens.filter(_.potentialIngredients.size == 1)
    var ambiguous = allergens.filter(_.potentialIngredients.size > 1)

    while(ambiguous.nonEmpty) {
      ambiguous = ambiguous.map(a => {
        a.copy(potentialIngredients = a.potentialIngredients diff definitive.flatMap(_.potentialIngredients).toSet)
      })
      definitive ++= ambiguous.filter(_.potentialIngredients.size == 1)
      ambiguous = ambiguous.filter(_.potentialIngredients.size > 1)
    }

    definitive.sortBy(_.name).map(_.potentialIngredients.head).mkString(",")
  }

  private case class Food(ingredients: Set[String], allergens: Set[String])

  private case class Allergen(name: String, potentialIngredients: Set[String])

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

  private def mapIngredientsToAllergens(food: List[Food]): List[Allergen] = {
    val allergens = food.flatMap(_.allergens).distinct
    allergens
      .map(allergen => {
        val potentialIngredients = food
          .filter(_.allergens.contains(allergen))
          .map(_.ingredients.toSet)
          .reduce(_ intersect _)
        Allergen(allergen, potentialIngredients)
      })
  }
}
