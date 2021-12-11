package adventofcode.year2021

import cats.effect._
import adventofcode._

object Day11 extends IOApp {

  case class Octopus(energy: Int, flashed: Boolean)

  case class Input(map: Grid[Octopus])

  case object Input {
    def from(input: List[String]): Input = {
      val rows = input.length
      val cols = input.head.length
      val data = for {
        (line, row) <- input.zipWithIndex
        (value, col) <- line.zipWithIndex
      } yield GridLocation(row, col) -> Octopus(Integer.parseInt(value.toString), false)
      Input(Grid(rows, cols, data.toMap))
    }
  }

  case class Context(process: Input => Long)

  object Runner extends Day[Input, Context, Long](2021, 11) {
    override def transformInput(lines: List[String]): Input =
      Input.from(lines)

    override def partOneContext(): Option[Context] =
      Some(Context(processPartOne))

    override def partTwoContext(): Option[Context] =
      Some(Context(processPartTwo))

    override def process(input: Input, context: Option[Context]): Option[Long] =
      context.map(_.process(input))

    private def processPartOne(input: Input): Long = {
      @annotation.tailrec
      def loop(i: Int, count: Long, map: Grid[Octopus]): Long =
        if (i == 100) count
        else {
          val nextMap = step(map)
          loop(i + 1, count + nextMap.data.count(_._2.flashed), nextMap)
        }

      loop(0, 0, input.map)
    }

    private def incrementLocations(locations: Iterable[GridLocation], map: Grid[Octopus]) =
      locations.foldLeft(map)((map, loc) =>
        map.updated(loc, map(loc.row, loc.col).copy(energy = map(loc.row, loc.col).energy + 1))
      )

    private def resetFlash(map: Grid[Octopus]) =
      map.allLocations.foldLeft(map)((map, loc) => map.updated(loc, map(loc.row, loc.col).copy(flashed = false)))

    private def processPartTwo(input: Input): Long = {
      @annotation.tailrec
      def loop(i: Long, map: Grid[Octopus]): Long =
        if (map.data.forall(_._2.energy == 0)) i
        else loop(i + 1, step(map))

      loop(0, input.map)
    }

    def step(map: Grid[Octopus]): Grid[Octopus] = {
      val next = incrementLocations(map.allLocations, resetFlash(map))
      val toFlash = next.data.filter(_._2.energy > 9).map(_._1).toSet

      @annotation.tailrec
      def flash(q: Set[GridLocation], acc: Grid[Octopus]): Grid[Octopus] = {
        q.toList match {
          case head +: tail =>
            val neighbors = acc
              .diagonalNeighbors(head.row, head.col)
              .map(_.location)
            val nextAcc = incrementLocations(neighbors, acc)
            val toFlash = neighbors
              .map(n => n -> nextAcc(n.row, n.col))
              .filter(_._2.energy > 9)
              .filterNot(_._2.flashed)
              .map(_._1)
            flash(
              (tail ++ toFlash).toSet,
              nextAcc.updated(head, nextAcc(head.row, head.col).copy(flashed = true))
            )
          case _ =>
            map.allLocations.foldLeft(acc)((acc, loc) =>
              if (acc(loc.row, loc.col).energy > 9) acc.updated(loc, acc(loc.row, loc.col).copy(energy = 0)) else acc
            )
        }
      }

      flash(toFlash, next)
    }
  }

  override def run(args: List[String]): IO[ExitCode] = Runner.run(args)
}
