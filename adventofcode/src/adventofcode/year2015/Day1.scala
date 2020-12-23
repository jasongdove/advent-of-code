package adventofcode.year2015

import adventofcode.Day
import cats.effect._

object Day1 extends IOApp {
  case class Context(process: List[Long] => Option[Long])

  private def partOneSymbolToScore(c: Char): Long = c match {
    case '(' => 1
    case ')' => -1
    case _   => 0
  }

  private def processPartOne(input: List[Long]): Option[Long] =
    Some(input.foldLeft(0L)(_ + _))

  private def processPartTwo(input: List[Long]): Option[Long] =
    Some(input.foldLeft((0L, 0L))((a, b) => if (a._2 < 0) (a._1, a._2) else (a._1 + 1, a._2 + b))._1)

  object Runner extends Day[List[Long], Context, Long](2015, 1) {
    override def transformInput(lines: List[String]): List[Long] =
      lines.mkString.trim.map(partOneSymbolToScore).toList

    override def partOneContext(): Option[Context] =
      Some(Context(processPartOne))

    override def partTwoContext(): Option[Context] =
      Some(Context(processPartTwo))

    override def process(input: List[Long], context: Option[Context]): Option[Long] =
      context.flatMap(_.process(input))
  }

  override def run(args: List[String]): IO[ExitCode] = Runner.run(args)
}
