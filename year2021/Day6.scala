package adventofcode.year2021

import adventofcode.Day
import cats.effect._

object Day6 extends IOApp {

  case class Input(numbers: List[Int])

  case object Input {
    def from(input: List[String]): Input = {
      val numbers = input.head.split(',').map(_.toInt).toList
      Input(numbers)
    }
  }

  case class Context(days: Int)

  case class Lanternfish(
    zero: Long,
    one: Long,
    two: Long,
    three: Long,
    four: Long,
    five: Long,
    six: Long,
    seven: Long,
    eight: Long
  ) {
    def sum = zero + one + two + three + four + five + six + seven + eight
    def shift: Lanternfish =
      Lanternfish(one, two, three, four, five, six, seven + zero, eight, zero)
  }

  object Runner extends Day[Input, Context, Long](2021, 6) {
    override def transformInput(lines: List[String]): Input =
      Input.from(lines)

    override def partOneContext(): Option[Context] =
      Some(Context(80))

    override def partTwoContext(): Option[Context] =
      Some(Context(256))

    override def process(input: Input, context: Option[Context]): Option[Long] =
      context.map(ctx => process(input, ctx.days))

    private def process(input: Input, days: Int): Long = {
      @annotation.tailrec
      def loop(day: Int, acc: Lanternfish): Lanternfish =
        if (day == days) acc else loop(day + 1, acc.shift)

      val start = Lanternfish(
        input.numbers.count(_ == 0).toLong,
        input.numbers.count(_ == 1).toLong,
        input.numbers.count(_ == 2).toLong,
        input.numbers.count(_ == 3).toLong,
        input.numbers.count(_ == 4).toLong,
        input.numbers.count(_ == 5).toLong,
        input.numbers.count(_ == 6).toLong,
        input.numbers.count(_ == 7).toLong,
        input.numbers.count(_ == 8).toLong
      )

      loop(0, start).sum
    }
  }

  override def run(args: List[String]): IO[ExitCode] = Runner.run(args)
}
