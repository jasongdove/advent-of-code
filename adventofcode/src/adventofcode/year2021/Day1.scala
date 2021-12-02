package adventofcode.year2021

import adventofcode.Day
import cats.effect._

object Day1 extends IOApp {

  case class Context(preprocess: List[Int] => List[Int])

  object Runner extends Day[List[Int], Context, Int](2021, 1) {
    override def transformInput(lines: List[String]): List[Int] =
      lines.flatMap(_.toIntOption)

    override def partOneContext(): Option[Context] =
      Some(Context(identity))

    override def partTwoContext(): Option[Context] =
      Some(Context(lst => lst.sliding(3).map(_.sum).toList))

    override def process(input: List[Int], context: Option[Context]): Option[Int] =
      context.map { ctx =>
        ctx.preprocess(input).sliding(2).count(lst => lst(1) > lst(0))
      }
  }

  override def run(args: List[String]): IO[ExitCode] = Runner.run(args)
}
