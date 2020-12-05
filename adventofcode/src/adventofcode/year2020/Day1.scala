package adventofcode.year2020

import adventofcode.Day

case class Day1Context(size: Int, total: Int)

object Day1 extends Day[Set[Long], Day1Context](2020, 1) {
  override def transformInput(lines: List[String]): Set[Long] =
    lines.map(_.toLongOption).flatten.toSet

  override def partOneContext(): Option[Day1Context] =
    Some(Day1Context(2, 2020))

  override def partTwoContext(): Option[Day1Context] =
    Some(Day1Context(3, 2020))

  override def process(input: Set[Long], context: Option[Day1Context]): Option[Long] =
    context.map { ctx =>
      input.subsets(ctx.size).find(_.sum == ctx.total).toList.flatten.product
    }
}
