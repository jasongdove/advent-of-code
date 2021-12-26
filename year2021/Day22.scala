// using java-opt -Xms128m
// using java-opt -Xmx4g

package adventofcode.year2021

import cats.effect._
import adventofcode._

object Day22 extends IOApp {

  case class RebootStep(isOn: Boolean, cuboid: Cuboid)

  case class Cuboid(x1: Int, y1: Int, z1: Int, x2: Int, y2: Int, z2: Int) {
    def contains(x: Int, y: Int, z: Int): Boolean =
      x >= x1 && x <= x2 &&
        y >= y1 && y <= y2 &&
        z >= z1 && z <= z2
  }

  object Cuboid {
    def from(min: Point3d, max: Point3d) =
      Cuboid(min.x, min.y, min.z, max.x, max.y, max.z)
  }

  case class Input(steps: List[RebootStep])

  object Input {
    val pattern = "(on|off) x=(-?\\d+)..(-?\\d+),y=(-?\\d+)..(-?\\d+),z=(-?\\d+)..(-?\\d+)".r
    def from(input: List[String]): Input = {
      val steps = input.map { line =>
        line match {
          case pattern(onoff, minX, maxX, minY, maxY, minZ, maxZ) =>
            val cuboid = Cuboid.from(
              Point3d(minX.toInt, minY.toInt, minZ.toInt),
              Point3d(maxX.toInt, maxY.toInt, maxZ.toInt)
            )
            RebootStep(onoff == "on", cuboid)
        }
      }
      Input(steps)
    }
  }

  case class Context(limit: Int)

  object Runner extends Day[Input, Context, Long](2021, 22) {
    override def transformInput(lines: List[String]): Input =
      Input.from(lines)

    override def partOneContext(): Option[Context] =
      Some(Context(50))

    override def partTwoContext(): Option[Context] =
      Some(Context(Int.MaxValue))

    override def process(input: Input, context: Option[Context]): Option[Long] = context.map { ctx =>
      val minX = math.max(input.steps.map(_.cuboid.x1).min, -ctx.limit)
      val minY = math.max(input.steps.map(_.cuboid.y1).min, -ctx.limit)
      val minZ = math.max(input.steps.map(_.cuboid.z1).min, -ctx.limit)
      val maxX = math.min(input.steps.map(_.cuboid.x2).max, ctx.limit)
      val maxY = math.min(input.steps.map(_.cuboid.y2).max, ctx.limit)
      val maxZ = math.min(input.steps.map(_.cuboid.z2).max, ctx.limit)

      var count = 0L

      val reversed = input.steps.reverse

      for {
        x <- minX to maxX
        y <- minY to maxY
        z <- minZ to maxZ
      } {
        count += (reversed.find(_.cuboid.contains(x, y, z)) match {
          case Some(RebootStep(true, _)) => 1
          case _                         => 0
        })
      }

      count
    }
  }

  override def run(args: List[String]): IO[ExitCode] = Runner.run(args)
}
