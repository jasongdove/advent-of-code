package adventofcode.year2016

import adventofcode._
import adventofcode.utils._
import cats.effect._

object Day15 extends IOApp {

  case class Disc(number: Int, positions: Int, time: Int, currentPosition: Int) {
    def isOpen(atTime: Int): Boolean =
      (currentPosition + (atTime - time)) % positions == 0
  }

  case class Sculpture(discs: List[Disc]) {
    def solve: Option[Int] =
      LazyList
        .from(1)
        .find(time => discs.forall(d => d.isOpen(time + d.number)))
  }

  case class Input(sculpture: Sculpture)

  case object Input {
    private val discPattern = "Disc #(\\d+) has (\\d+) positions; at time=(\\d+), it is at position (\\d+).".r

    def from(input: List[String]): Input = {
      val discs = input.map(line =>
        line match {
          case discPattern(number, positions, time, currentPosition) =>
            Disc(number.toInt, positions.toInt, time.toInt, currentPosition.toInt)
        }
      )
      Input(Sculpture(discs))
    }
  }

  case class Context(extraDisc: Option[Disc])

  object Runner extends Day[Input, Context, Int](2016, 15) {
    override def transformInput(lines: List[String]): Input =
      Input.from(lines)

    override def partOneContext(): Option[Context] =
      Some(Context(None))

    override def partTwoContext(): Option[Context] =
      Some(Context(Some(Disc(7, 11, 0, 0))))

    override def process(input: Input, context: Option[Context]): Option[Int] =
      context.flatMap(ctx => input.sculpture.copy(discs = input.sculpture.discs ++ ctx.extraDisc).solve)
  }

  override def run(args: List[String]): IO[ExitCode] = Runner.run(args)
}
