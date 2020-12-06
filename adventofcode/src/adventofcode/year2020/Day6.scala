package adventofcode.year2020

import adventofcode.Day

case class Day6Context(process: List[String] => Long)

object Day6 extends Day[List[String], Day6Context, Long](2020, 6) {
  override def transformInput(lines: List[String]): List[String] = lines

  override def splitOn(): String = "\n\n"

  override def partOneContext(): Option[Day6Context] =
    Some(Day6Context(processPartOne))

  override def partTwoContext(): Option[Day6Context] =
    Some(Day6Context(processPartTwo))

  override def process(input: List[String], context: Option[Day6Context]): Option[Long] = context.map(_.process(input))

  private def processPartOne(input: List[String]): Long =
    input.map(_.replaceAll("\\s", "").toList.distinct.length).sum.toLong

  private def processPartTwo(input: List[String]): Long =
    input
      .map { g =>
        {
          val group = g.split("\n").toList
          group.head.count(c => group.tail.forall(_.contains(c)))
        }
      }
      .sum
      .toLong

}
