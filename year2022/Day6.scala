package adventofcode.year2022

import adventofcode.Day
import cats.effect._

object Day6 extends IOApp {

  case class Context(solve: String => Int)

  object Runner extends Day[String, Context, Int](2022, 6) {
    override def transformInput(lines: List[String]): String = lines.head

    override def partOneContext(): Option[Context] =
      Some(Context(l => {
        val marker = l.sliding(4).find(s => s.distinct.length() == 4).get
        l.indexOf(marker) + 4
      }))

    override def partTwoContext(): Option[Context] =
      Some(Context(l => {
        val marker = l.sliding(14).find(s => s.distinct.length() == 14).get
        l.indexOf(marker) + 14
      }))

    override def process(input: String, context: Option[Context]): Option[Int] =
      context.map(_.solve(input))
  }

  override def run(args: List[String]): IO[ExitCode] = Runner.run(args)
}
