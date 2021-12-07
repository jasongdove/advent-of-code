package adventofcode.year2015

import adventofcode.Day
import cats.effect._

object Day20 extends IOApp {
  case class Context(presentsForHouse: Int => Int)

  private def elvesForHouse(number: Int, firstElf: Int): Iterable[Int] = {
    Range
      .inclusive(1, Math.sqrt(number.toDouble).toInt)
      .filter(number % _ == 0)
      .flatMap(i => List(i, number / i))
      .filter(_ >= firstElf)
  }

  private def presentsForHousePartOne(number: Int): Int =
    elvesForHouse(number, 1).map(_ * 10).sum

  private def presentsForHousePartTwo(number: Int): Int = {
    val firstElf = ((number - 1) / 50) + 1
    elvesForHouse(number, firstElf).map(_ * 11).sum
  }

  object Runner extends Day[Int, Context, Int](2015, 20) {

    override def transformInput(lines: List[String]): Int =
      lines.mkString.trim.toInt

    override def partOneContext(): Option[Context] =
      Some(Context(presentsForHousePartOne))

    override def partTwoContext(): Option[Context] =
      Some(Context(presentsForHousePartTwo))

    override def process(input: Int, context: Option[Context]): Option[Int] =
      context.flatMap { ctx =>
        LazyList
          .from(1)
          .find(house => ctx.presentsForHouse(house) >= input)
      }
  }

  override def run(args: List[String]): IO[ExitCode] = Runner.run(args)
}
