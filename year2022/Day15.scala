package adventofcode.year2022

import adventofcode.Day
import cats.effect._

object Day15 extends IOApp {
  case class Coordinate(x: Int, y: Int) {
    def distanceTo(other: Coordinate): Int =
      math.abs(x - other.x) + math.abs(y - other.y)
  }

  case class Line(from: Coordinate, to: Coordinate) {
    private val minx = math.min(from.x, to.x)
    private val maxx = math.max(from.x, to.x)
    private val miny = math.min(from.y, to.y)
    private val maxy = math.max(from.y, to.y)

    def points: Iterator[Coordinate] = {
      val dx = math.signum(to.x - from.x)
      val dy = math.signum(to.y - from.y)

      Iterator
        .from(0)
        .take(math.abs(from.x - to.x))
        .map(i => Coordinate(from.x + dx * i, from.y + dy * i))
    }
  }

  case class Pair(sensor: Coordinate, beacon: Coordinate) {
    val distance = sensor.distanceTo(beacon)
    val offsetLines: List[Line] = {
      val dist = distance + 1
      List(
        Line(Coordinate(sensor.x - dist, sensor.y), Coordinate(sensor.x, sensor.y - dist)),
        Line(Coordinate(sensor.x, sensor.y - dist), Coordinate(sensor.x + dist, sensor.y)),
        Line(Coordinate(sensor.x + dist, sensor.y), Coordinate(sensor.x, sensor.y + dist)),
        Line(Coordinate(sensor.x, sensor.y + dist), Coordinate(sensor.x - dist, sensor.y))
      )
    }

    def beaconNotPossibleAt(other: Coordinate) =
      sensor != beacon && other != beacon && sensor.distanceTo(other) <= distance
  }

  case class Input(target: Int, pairs: List[Pair])

  case object Input {
    private val pattern = "Sensor at x=([0-9\\-]+), y=([0-9\\-]+): closest beacon is at x=([0-9\\-]+), y=([0-9\\-]+)".r
    def from(lines: List[String]): Input = {
      val target = lines.head.toInt
      val pairs = lines.tail.map {
        _ match {
          case pattern(sx, sy, bx, by) =>
            Pair(Coordinate(sx.toInt, sy.toInt), Coordinate(bx.toInt, by.toInt))
        }
      }.toList
      Input(target, pairs)
    }
  }

  case class Context(solve: Input => Long)

  object Runner extends Day[Input, Context, Long](2022, 15) {
    override def transformInput(lines: List[String]): Input =
      Input.from(lines)

    override def partOneContext(): Option[Context] =
      Some(Context(input => {
        val minx = input.pairs.map(p => p.sensor.x - p.distance).min
        val maxx = input.pairs.map(p => p.sensor.x + p.distance).max
        (minx to maxx)
          .map(Coordinate(_, input.target))
          .count(c => input.pairs.exists(_.beaconNotPossibleAt(c)))
      }))

    override def partTwoContext(): Option[Context] =
      Some(Context(input => {
        val minx = math.max(input.pairs.map(p => p.sensor.x - p.distance).min, 0)
        val maxx = math.min(input.pairs.map(p => p.sensor.x + p.distance).max, input.target * 2)

        val miny = 0
        val maxy = input.target * 2

        val winner = input.pairs.iterator.flatMap(_.offsetLines.iterator.flatMap(_.points))
          .filter(c => c.x >= minx && c.x <= maxx && c.y >= miny && c.y <= maxy)
          .filter(c => input.pairs.forall(_.beaconNotPossibleAt(c) == false))
          .distinct
          .toList
          .last

        winner.x * 4_000_000L + winner.y
      }))

    override def process(input: Input, context: Option[Context]): Option[Long] =
      context.map(_.solve(input))
  }

  override def run(args: List[String]): IO[ExitCode] = Runner.run(args)
}
