package adventofcode.year2022

import adventofcode.Day
import cats.effect._

object Day16 extends IOApp {
  case class Input(whatever: List[Int])

  case object Input {
    def from(lines: List[String]): Input = {
      Input(List.empty)
    }
  }

  case class Context(solve: Input => Long)

  object Runner extends Day[Input, Context, Long](2022, 16) {
    override def transformInput(lines: List[String]): Input =
      Input.from(lines)

    override def partOneContext(): Option[Context] =
      Some(Context(input => {
        val _ = input
        0
      }))

    override def partTwoContext(): Option[Context] =
      Some(Context(input => {
        val _ = input
        0
      }))

    override def process(input: Input, context: Option[Context]): Option[Long] =
      context.map(_.solve(input))
  }

  override def run(args: List[String]): IO[ExitCode] = Runner.run(args)
}
