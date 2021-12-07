package adventofcode.year2021

import adventofcode.Day
import cats.effect._

object Day7 extends IOApp {

  case class Input(numbers: List[Int])

  case object Input {
    def from(input: List[String]): Input = {
      val numbers = input.head.split(',').map(_.toInt).toList
      Input(numbers)
    }
  }

  case class Context(cost: (Int, Int) => Int)

  object Runner extends Day[Input, Context, Int](2021, 7) {
    override def transformInput(lines: List[String]): Input =
      Input.from(lines)

    override def partOneContext(): Option[Context] =
      Some(Context { case (a, b) => Math.abs(a - b) })

    override def partTwoContext(): Option[Context] =
      Some(Context { case (a, b) => (Math.abs(a - b) to 1 by -1).sum })

    override def process(input: Input, context: Option[Context]): Option[Int] =
      context.map(ctx => process(input, ctx.cost))

    private def process(input: Input, cost: (Int, Int) => Int): Int =
      (input.numbers.min to input.numbers.max)
        .map(target => input.numbers.map(n => cost(n, target)).sum)
        .min
  }

  override def run(args: List[String]): IO[ExitCode] = Runner.run(args)
}
