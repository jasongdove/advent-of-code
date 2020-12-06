package adventofcode.year2015

import adventofcode.Day

case class Container(id: Int, liters: Int)
case class Day17Context(liters: Int, aggregate: List[List[Container]] => Int)

object Day17 extends Day[List[Container], Day17Context, Int](2015, 17) {

  override def transformInput(lines: List[String]): List[Container] =
    lines.zipWithIndex.map { case (l, i) => Container(i, l.toInt) }

  override def partOneContext(): Option[Day17Context] =
    Some(Day17Context(150, _.length))

  override def partTwoContext(): Option[Day17Context] =
    Some(Day17Context(150, minBucketOptions))

  override def process(input: List[Container], context: Option[Day17Context]): Option[Int] = context.map { ctx =>
    ctx.aggregate(
      Range(1, input.length + 1)
        .flatMap(i => input.combinations(i))
        .toList
        .filter(_.map(_.liters).sum == ctx.liters)
    )
  }

  def minBucketOptions(allOptions: List[List[Container]]): Int = {
    val minOption = allOptions.sortWith(_.length < _.length).head.length
    allOptions.count(_.length == minOption)
  }
}
