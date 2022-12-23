package adventofcode.year2022

import adventofcode.Day
import cats.effect._

object Day18 extends IOApp {
  val canReachOutsideMemo = scala.collection.mutable.Set.empty[Cube]

  case class Face(x1: Double, y1: Double, z1: Double, x2: Double, y2: Double, z2: Double)

  case class Cube(x: Int, y: Int, z: Int) {
    def faces(): List[Face] = List(
      Face(x - 0.5, y + 0.5, z - 0.5, x + 0.5, y - 0.5, z - 0.5),
      Face(x - 0.5, y + 0.5, z + 0.5, x + 0.5, y - 0.5, z + 0.5),
      Face(x - 0.5, y + 0.5, z + 0.5, x - 0.5, y - 0.5, z - 0.5),
      Face(x + 0.5, y + 0.5, z + 0.5, x + 0.5, y - 0.5, z - 0.5),
      Face(x - 0.5, y + 0.5, z + 0.5, x + 0.5, y + 0.5, z - 0.5),
      Face(x - 0.5, y - 0.5, z + 0.5, x + 0.5, y - 0.5, z - 0.5)
    )

    def canReachOutside(
      cubes: Set[Cube],
      minx: Int,
      maxx: Int,
      miny: Int,
      maxy: Int,
      minz: Int,
      maxz: Int
    ): Boolean = {
      def priorityOrder(c: Cube): Int = -1 * (math.min(math.abs(c.x - minx), math.abs(c.x - maxx)) +
        math.min(math.abs(c.y - miny), math.abs(c.y - maxy)) +
        math.min(math.abs(c.z - minz), math.abs(c.z - maxz)))

      val q = scala.collection.mutable.PriorityQueue(this)(Ordering.by(priorityOrder))
      val visited = scala.collection.mutable.Set.empty[Cube]

      while (!q.isEmpty) {
        val cube = q.dequeue()
        if (!visited.contains(cube)) {
          visited.addOne(cube)
          if (
            canReachOutsideMemo.contains(
              cube
            ) || ((cube.x < minx || cube.x > maxx) && (cube.y < miny || cube.y > maxy) && (cube.z < minz || cube.z > maxz))
          ) {
            canReachOutsideMemo.addAll(visited)
            return true;
          }
          val next = List(
            Cube(cube.x - 1, cube.y, cube.z),
            Cube(cube.x + 1, cube.y, cube.z),
            Cube(cube.x, cube.y - 1, cube.z),
            Cube(cube.x, cube.y + 1, cube.z),
            Cube(cube.x, cube.y, cube.z - 1),
            Cube(cube.x, cube.y, cube.z + 1)
          ).filterNot(cubes.contains).filterNot(visited.contains)
          q.addAll(next)
        }
      }

      false
    }
  }

  case class Input(cubes: List[Cube])

  case object Input {
    def from(lines: List[String]): Input =
      Input(lines.map { l =>
        val coords = l.split(",").map(_.toInt).toList
        Cube(coords(0), coords(1), coords(2))
      })
  }

  def area(cubes: Seq[Cube]): Int =
    cubes
      .flatMap(_.faces)
      .groupBy(identity)
      .view
      .mapValues(_.size)
      .filter(_._2 == 1)
      .size

  case class Context(solve: Input => Int)

  object Runner extends Day[Input, Context, Int](2022, 18) {
    override def transformInput(lines: List[String]): Input =
      Input.from(lines)

    override def partOneContext(): Option[Context] =
      Some(Context(input => area(input.cubes)))

    override def partTwoContext(): Option[Context] =
      Some(Context(input => {
        val minx = input.cubes.map(_.x).min
        val maxx = input.cubes.map(_.x).max
        val miny = input.cubes.map(_.y).min
        val maxy = input.cubes.map(_.y).max
        val minz = input.cubes.map(_.z).min
        val maxz = input.cubes.map(_.z).max

        val cubesToCheck = (for {
          x <- minx to maxx
          y <- miny to maxy
          z <- minz to maxz
        } yield Cube(x, y, z)).filterNot(input.cubes.contains)

        val cubesSet = input.cubes.toSet

        val internalMissingCubes = cubesToCheck.filterNot(
          _.canReachOutside(
            cubesSet,
            minx,
            maxx,
            miny,
            maxy,
            minz,
            maxz
          )
        )

        area(input.cubes ++ internalMissingCubes)
      }))

    override def process(input: Input, context: Option[Context]): Option[Int] =
      context.map(_.solve(input))
  }

  override def run(args: List[String]): IO[ExitCode] = Runner.run(args)
}
