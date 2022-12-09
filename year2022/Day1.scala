package adventofcode.year2022

import adventofcode.Day
import cats.effect._

object Day1 extends IOApp {

  case class Elf(food: List[Int])

  case class Context(solve: List[Elf] => Int)

  object Runner extends Day[List[Elf], Context, Int](2022, 1) {
    override def transformInput(lines: List[String]): List[Elf] = {
      var result = scala.collection.mutable.ArrayBuffer.empty[Elf]
      var mut = scala.collection.mutable.ArrayBuffer.empty[String]
      for (c: String <- lines) {
        if (c.isBlank()) {
          result.addOne(Elf(mut.flatMap(_.toIntOption).toList))
          mut.clear()
        } else {
          mut.addOne(c)
        }
      }

      if (mut.nonEmpty) {
        result.addOne(Elf(mut.flatMap(_.toIntOption).toList))
      }

      result.toList
    }

    override def partOneContext(): Option[Context] =
      Some(Context(_.map(_.food.sum).max))

    override def partTwoContext(): Option[Context] =
      Some(Context(_.map(_.food.sum).sorted.reverse.take(3).sum))

    override def process(input: List[Elf], context: Option[Context]): Option[Int] =
      context.map(_.solve(input))
  }

  override def run(args: List[String]): IO[ExitCode] = Runner.run(args)
}
