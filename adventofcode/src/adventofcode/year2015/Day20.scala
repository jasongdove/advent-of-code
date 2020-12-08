package adventofcode.year2015

import adventofcode.Day

case class Day20Context(presentsForHouse: Int => Int)

object Day20 extends Day[Int, Day20Context, Int](2015, 20) {

  override def transformInput(lines: List[String]): Int =
    lines.mkString.trim.toInt

  override def partOneContext(): Option[Day20Context] =
    Some(Day20Context(presentsForHousePartOne))

  override def partTwoContext(): Option[Day20Context] =
    Some(Day20Context(presentsForHousePartTwo))

  override def process(input: Int, context: Option[Day20Context]): Option[Int] =
    context.flatMap { ctx =>
      LazyList
        .from(1)
        .find(house => ctx.presentsForHouse(house) >= input)
    }

  private def elvesForHouse(number: Int, firstElf: Int): Set[Int] = {
    Range
      .inclusive(1, Math.sqrt(number.toDouble).toInt)
      .filter(number % _ == 0)
      .flatMap(i => List(i, number / i))
      .filter(_ >= firstElf)
      .toSet
  }

  private def presentsForHousePartOne(number: Int): Int =
    elvesForHouse(number, 1).map(_ * 10).sum

  private def presentsForHousePartTwo(number: Int): Int = {
    val firstElf = ((number - 1) / 50) + 1
    elvesForHouse(number, firstElf).map(_ * 11).sum
  }
}
