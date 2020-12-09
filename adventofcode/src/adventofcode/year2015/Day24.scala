package adventofcode.year2015

import adventofcode.Day

case class Day24Context(groups: Int)

object Day24 extends Day[Set[Long], Day24Context, Long](2015, 24) {

  override def transformInput(lines: List[String]): Set[Long] =
    lines.map(_.toLong).toSet

  override def partOneContext(): Option[Day24Context] =
    Some(Day24Context(3))

  override def partTwoContext(): Option[Day24Context] =
    Some(Day24Context(4))

  override def process(input: Set[Long], context: Option[Day24Context]): Option[Long] =
    context.map { ctx =>
      val targetWeight = input.sum / ctx.groups
      val fewest = fewestEqualWeight(input, targetWeight)
      input.subsets(fewest).filter(_.sum == targetWeight).map(_.product).min
    }

  private def fewestEqualWeight(input: Set[Long], weight: Long): Int = {
    (1 to input.size).foldLeft(0) { (acc, i) =>
      if (acc > 0) acc
      else if (input.subsets(i).exists(_.sum == weight)) i
      else 0
    }
  }
}
