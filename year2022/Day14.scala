package adventofcode.year2022

import adventofcode.Day
import cats.effect._

object Day14 extends IOApp {
  private val sandStart = Coordinate(500, 0)

  case class Coordinate(x: Int, y: Int) {
    def down = Coordinate(x, y + 1)
    def downLeft = Coordinate(x - 1, y + 1)
    def downRight = Coordinate(x + 1, y + 1)
  }

  case class RockPath(from: Coordinate, to: Coordinate) {
    private val minx = math.min(from.x, to.x)
    private val maxx = math.max(from.x, to.x)
    private val miny = math.min(from.y, to.y)
    private val maxy = math.max(from.y, to.y)

    def coords: List[Coordinate] =
      (for {
        x <- minx to maxx
        y <- miny to maxy
      } yield Coordinate(x, y)).toList
  }

  case object RockPath {
    def pathsFrom(line: String): List[RockPath] =
      line
        .split(" -> ")
        .map(s => {
          val split = s.split(',').map(_.toInt)
          Coordinate(split(0), split(1))
        })
        .sliding(2)
        .map(locs => RockPath(locs(0), locs(1)))
        .toList
  }

  case class Problem(rockPaths: List[RockPath]) {
    private val sand = collection.mutable.Set.empty[Coordinate]
    private val rocks: Set[Coordinate] = rockPaths.flatMap(_.coords).toSet

    private val minx = (rocks.map(_.x) + sandStart.x).min
    private val maxx = (rocks.map(_.x) + sandStart.x).max
    private val miny = (rocks.map(_.y) + sandStart.y).min
    private val maxy = (rocks.map(_.y) + sandStart.y).max

    def intersectsAny(intersectsCheck: (Problem, Coordinate) => Boolean, coord: Coordinate): Boolean =
      intersectsCheck(this, coord) || rocks.contains(coord) || sand.contains(coord)

    def intersectsFloor(coord: Coordinate): Boolean = coord.y == maxy + 2
    def sandContainsSandStart(): Boolean = sand.contains(sandStart)
    def sandIsFallingForever(coord: Coordinate) = coord.y > maxy

    def addSand(
      stopAddingSand: (Problem, Coordinate) => Boolean,
      intersectsCheck: (Problem, Coordinate) => Boolean
    ): Boolean = {
      @annotation.tailrec
      def moveSand(coord: Coordinate): (Coordinate, Boolean) = {
        if (stopAddingSand(this, coord)) (coord, false)
        else if (intersectsAny(intersectsCheck, coord.down) == false) moveSand(coord.down)
        else if (intersectsAny(intersectsCheck, coord.downLeft) == false) moveSand(coord.downLeft)
        else if (intersectsAny(intersectsCheck, coord.downRight) == false) moveSand(coord.downRight)
        else (coord, true)
      }

      val (nextCoord: Coordinate, cameToRest: Boolean) = moveSand(sandStart)
      if (cameToRest) {
        sand.addOne(nextCoord)
      }
      cameToRest
    }

    def printMap(): Unit = {
      for (y <- miny to maxy) {
        for (x <- minx to maxx) {
          val coord = Coordinate(x, y)
          if (coord == sandStart) print("+")
          else if (sand.contains(coord)) print("o")
          else if (rocks.contains(coord)) print("#")
          else print(".")
        }
        println()
      }
    }
  }

  case class Context(
    stopAddingSand: (Problem, Coordinate) => Boolean,
    intersectsCheck: (Problem, Coordinate) => Boolean
  )

  object Runner extends Day[Problem, Context, Int](2022, 14) {
    override def transformInput(lines: List[String]): Problem =
      Problem(lines.flatMap(RockPath.pathsFrom))

    override def partOneContext(): Option[Context] =
      Some(Context((p, c) => p.sandIsFallingForever(c), (_, _) => false))

    override def partTwoContext(): Option[Context] =
      Some(Context((p, c) => p.sandContainsSandStart(), (p, c) => p.intersectsFloor(c)))

    override def process(input: Problem, context: Option[Context]): Option[Int] =
      context.map { ctx =>
        LazyList
          .iterate(true)(_ => input.addSand(ctx.stopAddingSand, ctx.intersectsCheck))
          .indexWhere(_ == false) - 1
      }
  }

  override def run(args: List[String]): IO[ExitCode] = Runner.run(args)
}
