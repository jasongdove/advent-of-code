package adventofcode.year2015

import adventofcode.Day

case class Day1Context(process: List[Long] => Option[Long])

object Day1 extends Day[List[Long], Day1Context](2015, 1) {
  private def partOneSymbolToScore(c: Char): Long = c match {
    case '(' => 1
    case ')' => -1
    case _   => 0
  }

  override def transformInput(lines: List[String]): List[Long] =
    lines.mkString.trim.map(partOneSymbolToScore).toList

  override def partOneContext(): Option[Day1Context] =
    Some(Day1Context(processPartOne))

  override def partTwoContext(): Option[Day1Context] =
    Some(Day1Context(processPartTwo))

  override def process(input: List[Long], context: Option[Day1Context]): Option[Long] =
    context.flatMap(_.process(input))

  private def processPartOne(input: List[Long]): Option[Long] =
    Some(input.foldLeft(0L)(_ + _))

  private def processPartTwo(input: List[Long]): Option[Long] =
    Some(input.foldLeft((0L, 0L))((a, b) => if (a._2 < 0) (a._1, a._2) else (a._1 + 1, a._2 + b))._1)
}
