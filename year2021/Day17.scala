// using java-opt -Xms64m
// using java-opt -Xmx1g

package adventofcode.year2021

import cats.effect._
import adventofcode._

object Day17 extends IOApp {

  case class Probe(x: Int, y: Int, vx: Int, vy: Int) {
    def step(): Probe = {
      val nextVx = vx match {
        case 0              => 0
        case pos if pos > 0 => pos - 1
        case neg if neg < 0 => neg + 1
      }
      Probe(x + vx, y + vy, nextVx, vy - 1)
    }

    def isWithin(targetArea: TargetArea): Boolean =
      x >= targetArea.xmin && x <= targetArea.xmax &&
        y >= targetArea.ymin && y <= targetArea.ymax
  }

  case class TargetArea(xmin: Int, xmax: Int, ymin: Int, ymax: Int)

  case class Input(targetArea: TargetArea)

  case object Input {
    val pattern = "target area: x=(-?\\d+)..(-?\\d+), y=(-?\\d+)..(-?\\d+)".r

    def from(input: List[String]): Input = {
      val targetArea = input.head match {
        case pattern(xmin, xmax, ymin, ymax) =>
          TargetArea(xmin.toInt, xmax.toInt, ymin.toInt, ymax.toInt)
      }
      Input(targetArea)
    }
  }

  case class Context(aggregate: Seq[Vector[Probe]] => Long)

  object Runner extends Day[Input, Context, Long](2021, 17) {
    override def transformInput(lines: List[String]): Input =
      Input.from(lines)

    override def partOneContext(): Option[Context] =
      Some(Context(_.map(_.map(_.y).max).max))

    override def partTwoContext(): Option[Context] =
      Some(Context(_.size))

    override def process(input: Input, context: Option[Context]): Option[Long] = {
      context.map { ctx =>
        val probes = for {
          vx <- -300 to 300
          vy <- -300 to 300
        } yield {
          Probe(0, 0, vx, vy)
        }

        val throughTarget = probes.flatMap { probe =>
          val steps = (1 to 500).foldLeft(Vector(probe))((v, _) => v :+ v.last.step())
          if (steps.exists(_.isWithin(input.targetArea))) Some(steps) else None
        }

        ctx.aggregate(throughTarget)
      }
    }

  }

  override def run(args: List[String]): IO[ExitCode] = Runner.run(args)
}
