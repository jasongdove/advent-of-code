package adventofcode.year2022

import adventofcode.{Day, Grid, GridLocation}
import cats.effect._

object Day17 extends IOApp {

  sealed trait JetDirection

  case object Left extends JetDirection
  case object Right extends JetDirection

  case object JetDirection {
    def from(c: Char): JetDirection = {
      if (c != '<' && c != '>') {
        println("bad input!")
      }
      c match {
        case '<' => Left
        case '>' => Right
      }
    }
  }

  case class Coordinate(x: Int, y: Int)
  case class RockCoordinate(coordinate: Coordinate, hasRock: Boolean)

  case class Rock(coordinates: List[RockCoordinate], width: Int, height: Int) {
    def from(coordinate: Coordinate): List[Coordinate] =
      coordinates
        .filter(_.hasRock)
        .map(rc => Coordinate(coordinate.x + rc.coordinate.x, coordinate.y + rc.coordinate.y))
  }

  case object Rock {
    def from(lines: List[String]): Rock = {
      val coordinates = for {
        y <- 0 until lines.size
        x <- 0 until lines.head.size
      } yield RockCoordinate(Coordinate(x, -y), lines(y)(x) == '#')
      Rock(coordinates.toList, lines.head.size, lines.size)
    }
  }

  case class Problem(rocks: List[Rock], jets: List[JetDirection])

  case object Problem {
    def from(lines: List[String]): Problem = {
      val jetsLine = lines.head.trim()
      val rocksLines = lines.tail.mkString("\n").split("\n\n").map(_.split("\n").toList).toList
      val jetsList = jetsLine.toList.map(JetDirection.from)
      val rocksList = rocksLines.map(Rock.from)
      Problem(rocksList, jetsList)
    }
  }

  def solve(problem: Problem, rocksToDrop: Int): List[Int] = {
    val result = scala.collection.mutable.ArrayBuffer.empty[Int]

    case class RockLocation(loc: Coordinate, rock: Rock) {
      def coords = rock.from(loc)

      def push(direction: JetDirection, chamber: Seq[Coordinate]): RockLocation = {
        direction match {
          case Left =>
            if (
              rock.coordinates
                .filter(_.hasRock)
                .map(rc => Coordinate(loc.x + rc.coordinate.x - 1, loc.y + rc.coordinate.y))
                .exists(c => c.x < 0 || chamber.contains(c))
            ) this
            else RockLocation(Coordinate(loc.x - 1, loc.y), rock)
          case Right =>
            if (
              rock.coordinates
                .filter(_.hasRock)
                .map(rc => Coordinate(loc.x + rc.coordinate.x + 1, loc.y + rc.coordinate.y))
                .exists(c => c.x >= 7 || chamber.contains(c))
            ) this
            else RockLocation(Coordinate(loc.x + 1, loc.y), rock)
        }
      }

      def moveDown(): RockLocation = RockLocation(Coordinate(loc.x, loc.y - 1), rock)

      def intersects(chamber: scala.collection.mutable.IndexedSeq[Coordinate]): Boolean =
        rock.coordinates.exists(_.coordinate.y + loc.y < 0) || rock.coordinates
          .filter(_.hasRock)
          .map(rc => Coordinate(loc.x + rc.coordinate.x, loc.y + rc.coordinate.y))
          .exists(chamber.contains)
    }

    def printFallingChamber(chamber: Seq[Coordinate], falling: RockLocation): Unit = {
      val fallingLocs = falling.rock.from(falling.loc)
      for (y <- ((chamber.map(_.y) :+ 0) ++ fallingLocs.map(_.y)).max to 0 by -1) {
        print('|')
        for (x <- 0 until 7) {
          if (fallingLocs.contains(Coordinate(x, y))) print('@')
          else if (chamber.contains(Coordinate(x, y))) print('#')
          else print('.')
        }
        println('|')
      }
      println("+-------+")
      println()
    }

    def printFallingChamberCheck(
      chamber: Seq[Coordinate],
      falling: RockLocation,
      check: RockLocation
    ): Unit = {
      val fallingLocs = falling.rock.from(falling.loc)
      val checkLocs = check.rock.from(check.loc)
      for (y <- ((chamber.map(_.y) :+ 0) ++ fallingLocs.map(_.y)).max to 0 by -1) {
        print('|')
        for (x <- 0 until 7) {
          if (checkLocs.contains(Coordinate(x, y))) print('x')
          else if (fallingLocs.contains(Coordinate(x, y))) print('@')
          else if (chamber.contains(Coordinate(x, y))) print('#')
          else print('.')
        }
        println('|')
      }
      println("+-------+")
      println()
    }

    def printChamber(chamber: scala.collection.mutable.IndexedSeq[Coordinate]): Unit = {
      for (y <- (chamber.map(_.y) :+ 0).max to 0 by -1) {
        print('|')
        for (x <- 0 until 7) {
          if (chamber.contains(Coordinate(x, y))) print('#')
          else print('.')
        }
        println('|')
      }
      println("+-------+")
      println()
    }

    val chamber = scala.collection.mutable.ArrayBuffer.empty[Coordinate]
    var jetIndex = 0

    for (index <- 0 until rocksToDrop) {
      val highestRock = (chamber.map(_.y) :+ -1).max
      result.addOne(highestRock + 1)

      var r = problem.rocks(index % problem.rocks.size)
      var rloc = RockLocation(Coordinate(2, highestRock + 3 + r.height), r)

      // printFallingChamber(chamber, rloc)

      var done = false
      while (!done) {
        // printFallingChamber(chamber, rloc)
        rloc = rloc.push(problem.jets(jetIndex), chamber.toSeq)
        // printFallingChamber(chamber, rloc)
        jetIndex = (jetIndex + 1) % problem.jets.size
        val maybeDown = rloc.moveDown()
        // printFallingChamberCheck(chamber, rloc, maybeDown)
        if (!maybeDown.intersects(chamber)) {
          rloc = maybeDown
        } else {
          done = true
        }
      }

      chamber.addAll(rloc.coords)
    }

    // printChamber(chamber)

    result.addOne(chamber.map(_.y).max + 1)

    result.toList
  }

  private def solveCycle(
    input: Problem,
    blocks: Long,
    cycleLength: Int,
    cycleStart: Int,
    cycleHeight: Int,
    startHeight: Int
  ) = {
    val cycle = solve(input, (cycleStart + cycleLength).toInt)
    val x = blocks - cycleStart
    (startHeight + (x / cycleLength) * cycleHeight) + cycle(cycleStart + ((x % cycleLength)).toInt) - startHeight
  }

  case class Context(solve: Problem => Long)

  object Runner extends Day[Problem, Context, Long](2022, 17) {
    override def transformInput(lines: List[String]): Problem =
      Problem.from(lines)

    override def partOneContext(): Option[Context] =
      Some(Context(input => {
        solve(input, 2022).last
      }))

    override def partTwoContext(): Option[Context] =
      Some(Context(input => {
        // example: solveCycle(input, 1_000_000_000_000L, 35, 28, 53, 48)
        solveCycle(input, 1_000_000_000_000L, 1745, 950, 2767, 1489)
      }))

    override def process(input: Problem, context: Option[Context]): Option[Long] =
      context.map(_.solve(input))
  }

  override def run(args: List[String]): IO[ExitCode] = Runner.run(args)
}
