package adventofcode.year2015

import adventofcode.Day
import cats.effect._

object Day15 extends IOApp {
  case class Ingredient(name: String, capacity: Int, durability: Int, flavor: Int, texture: Int, calories: Int)

  object Ingredient {
    private val pattern =
      "(.*): capacity (-?\\d+), durability (-?\\d+), flavor (-?\\d+), texture (-?\\d+), calories (-?\\d+)".r
    def from(line: String): Ingredient = {
      val pattern(name, capacity, durability, flavor, texture, calories) = line
      Ingredient(name, capacity.toInt, durability.toInt, flavor.toInt, texture.toInt, calories.toInt)
    }
  }

  case class Context(teaspoons: Int, caloriesOk: Long => Boolean)

  object Runner extends Day[List[Ingredient], Context, Long](2015, 15) {

    override def transformInput(lines: List[String]): List[Ingredient] =
      lines.map(Ingredient.from)

    override def partOneContext(): Option[Context] =
      Some(Context(100, _ => true))

    override def partTwoContext(): Option[Context] =
      Some(Context(100, c => c == 500))

    override def process(input: List[Ingredient], context: Option[Context]): Option[Long] = {
      def scoreForProperty(imap: Map[Ingredient, Int], selector: Ingredient => Int): Long = {
        val score = imap.map { case (i, q) => selector(i).toLong * q }.sum
        if (score < 0) 0 else score
      }

      context.map { ctx =>
        input
          .flatMap(i => Range(0, ctx.teaspoons).map(_ => i))
          .combinations(ctx.teaspoons)
          .map { ingredients =>
            val imap = ingredients.groupBy(identity).view.mapValues(_.size).toMap
            val calories = scoreForProperty(imap, _.calories)
            if (ctx.caloriesOk(calories)) {
              val capacity = scoreForProperty(imap, _.capacity)
              val durability = scoreForProperty(imap, _.durability)
              val flavor = scoreForProperty(imap, _.flavor)
              val texture = scoreForProperty(imap, _.texture)
              capacity * durability * flavor * texture
            } else 0
          }
          .max
      }
    }
  }

  override def run(args: List[String]): IO[ExitCode] = Runner.run(args)
}
