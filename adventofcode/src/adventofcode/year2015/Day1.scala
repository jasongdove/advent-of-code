package adventofcode.year2015

import adventofcode.Day
import cats.effect._

object Day1 extends IOApp {
  case class Context(process: List[Int] => Option[Int])

  private def partOneSymbolToScore(c: Char): Int = c match {
    case '(' => 1
    case ')' => -1
    case _   => 0
  }

  private def processPartOne(input: List[Int]): Option[Int] =
    Some(input.foldLeft(0)(_ + _))

  private def processPartTwo(input: List[Int]): Option[Int] =
    Some(input.foldLeft((0, 0))((a, b) => if (a._2 < 0) (a._1, a._2) else (a._1 + 1, a._2 + b))._1)

  object Runner extends Day[List[Int], Context, Int](2015, 1) {
    override def transformInput(lines: List[String]): List[Int] =
      lines.mkString.trim.map(partOneSymbolToScore).toList

    override def partOneContext(): Option[Context] =
      Some(Context(processPartOne))

    override def partTwoContext(): Option[Context] =
      Some(Context(processPartTwo))

    override def process(input: List[Int], context: Option[Context]): Option[Int] =
      context.flatMap(_.process(input))
  }

  override def run(args: List[String]): IO[ExitCode] = Runner.run(args)
}
