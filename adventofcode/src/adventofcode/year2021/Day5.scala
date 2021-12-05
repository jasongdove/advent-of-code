package adventofcode.year2021

import adventofcode.Day
import cats.effect._

object Day5 extends IOApp {

  case class Point(x: Int, y: Int)

  case class PointRange(x1: Int, y1: Int, x2: Int, y2: Int) {
    def points(diagonal: Boolean): List[Point] = {
      if (x1 == x2) {
        val low = Integer.min(y1, y2)
        val hi = Integer.max(y1, y2)
        (low to hi).map(y => Point(x1, y)).toList
      } else if (y1 == y2) {
        val low = Integer.min(x1, x2)
        val hi = Integer.max(x1, x2)
        (low to hi).map(x => Point(x, y1)).toList
      } else if (diagonal) {
        val xstep = if (x1 - x2 < 0) 1 else -1
        val ystep = if (y1 - y2 < 0) 1 else -1
        (0 to Math.abs(x1 - x2))
          .map(i => Point(x1 + i * xstep, y1 + i * ystep))
          .toList
      } else {
        List.empty[Point]
      }
    }
  }

  case class Input(pointRanges: List[PointRange])

  case object Input {
    def from(input: List[String]): Input = {
      val ranges = input.map(line => {
        val sides = line.split(" -> ").toList
        val lefts = sides(0).split(',').map(_.toInt)
        val rights = sides(1).split(',').map(_.toInt)
        PointRange(lefts(0), lefts(1), rights(0), rights(1))
      })
      Input(ranges)
    }
  }

  case class Context(diagonal: Boolean)

  object Runner extends Day[Input, Context, Int](2021, 5) {
    override def transformInput(lines: List[String]): Input =
      Input.from(lines)

    override def partOneContext(): Option[Context] =
      Some(Context(false))

    override def partTwoContext(): Option[Context] =
      Some(Context(true))

    override def process(input: Input, context: Option[Context]): Option[Int] =
      context.map(ctx => {
        val pointFreq = input.pointRanges
          .flatMap(_.points(ctx.diagonal))
          .groupBy(identity)
          .view
          .mapValues(_.size)
          .toList

        pointFreq.count(_._2 > 1)
      })
  }

  override def run(args: List[String]): IO[ExitCode] = Runner.run(args)
}
