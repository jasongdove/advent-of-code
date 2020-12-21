package adventofcode.year2020

import adventofcode.Day

import scala.annotation.tailrec

case class Day21Food(ingredients: Set[String], allergens: List[String])

object Day21Food {
  private val pattern = "([a-z\\s]+) \\(contains ([a-z\\s,]+)\\)".r

  def from(line: String): Day21Food = {
    val pattern(ingredients, allergens) = line
    Day21Food(ingredients.split(" ").toSet, allergens.split(", ").toList)
  }
}

case class Day21Context(process: List[Day21Food] => String)

object Day21 extends Day[List[Day21Food], Day21Context, String](2020, 21) {

  case class AllergenPossibility(allergen: String, ingredients: List[String])
  case class KnownAllergen(ingredient: String, allergen: String)

  override def transformInput(lines: List[String]): List[Day21Food] =
    lines.map(Day21Food.from)

  override def partOneContext(): Option[Day21Context] =
    Some(Day21Context(processPartOne))

  override def partTwoContext(): Option[Day21Context] =
    Some(Day21Context(processPartTwo))

  override def process(input: List[Day21Food], context: Option[Day21Context]): Option[String] =
    context.map(_.process(input))

  private def processPartOne(foods: List[Day21Food]): String = {
    val allIngredients = foods.flatMap(_.ingredients)

    val maybeAllergens = foods
      .flatMap(_.allergens)
      .flatMap { allergen =>
        foods.filter(_.allergens.contains(allergen)).map(_.ingredients).reduce(_ intersect _)
      }
      .toSet

    val notAllergens = allIngredients.toSet diff maybeAllergens

    allIngredients.count(notAllergens.contains).toString
  }

  private def processPartTwo(foods: List[Day21Food]): String = {
    val maybeAllergens = foods
      .flatMap(_.allergens)
      .distinct
      .map { allergen =>
        val ingredients = foods.filter(_.allergens.contains(allergen)).map(_.ingredients).reduce(_ intersect _)
        AllergenPossibility(allergen, ingredients.toList)
      }

    val knownAllergens = identifyAllergens(maybeAllergens)

    knownAllergens.sortBy(_.allergen).map(_.ingredient).mkString(",")
  }

  def identifyAllergens(possibilities: List[AllergenPossibility]): List[KnownAllergen] = {
    @tailrec
    def loop(acc: List[KnownAllergen], remaining: List[AllergenPossibility]): List[KnownAllergen] = {
      remaining.sortBy(_.ingredients.length) match {
        // find possibility with single ingredient
        case AllergenPossibility(allergen, ingredient :: Nil) :: next =>
          val known = KnownAllergen(ingredient, allergen)
          // remove the ingredient from all other possibilities
          val removed = next.map(ap => AllergenPossibility(ap.allergen, ap.ingredients diff List(ingredient)))
          loop(known :: acc, removed)
        case _ => acc
      }
    }

    loop(List.empty, possibilities)
  }
}
