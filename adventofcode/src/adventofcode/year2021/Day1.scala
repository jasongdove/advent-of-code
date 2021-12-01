package adventofcode.year2021

import adventofcode.Day

case class Day1Context(preprocess: List[Int] => List[Int])

object Day1 extends Day[List[Int], Day1Context, Int](2021, 1) {
  override def transformInput(lines: List[String]): List[Int] =
    lines.flatMap(_.toIntOption)

  override def partOneContext(): Option[Day1Context] =
    Some(Day1Context(identity))

  override def partTwoContext(): Option[Day1Context] =
    Some(Day1Context(lst => lst.sliding(3).map(_.sum).toList))

  override def process(input: List[Int], context: Option[Day1Context]): Option[Int] =
    context.map { ctx =>
      ctx.preprocess(input).sliding(2).count(lst => lst(1) > lst(0))
    }
}
