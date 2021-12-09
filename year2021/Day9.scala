package adventofcode.year2021

import cats.effect._
import adventofcode._

object Day9 extends IOApp {

  case class Input(map: Grid[Int])

  case object Input {
    def from(input: List[String]): Input = {
      val rows = input.length
      val cols = input.head.length
      val data = for {
        (line, row) <- input.zipWithIndex
        (value, col) <- line.zipWithIndex
      } yield GridLocation(row, col) -> Integer.parseInt(value.toString)
      Input(Grid(rows, cols, data.toMap))
    }
  }

  case class Context(processLowPoints: (Grid[Int], List[GridLocation]) => Int)

  object Runner extends Day[Input, Context, Int](2021, 9) {
    override def transformInput(lines: List[String]): Input =
      Input.from(lines)

    override def partOneContext(): Option[Context] =
      Some(Context(processPartOne))

    override def partTwoContext(): Option[Context] =
      Some(Context(processPartTwo))

    override def process(input: Input, context: Option[Context]): Option[Int] =
      context.map(_.processLowPoints(input.map, lowPoints(input.map)))

    private def processPartOne(map: Grid[Int], lowPoints: List[GridLocation]): Int =
      lowPoints.map(lp => map(lp.row, lp.col) + 1).sum

    private def processPartTwo(map: Grid[Int], lowPoints: List[GridLocation]): Int = {
      @annotation.tailrec
      def loop(q: Vector[GridLocation], visited: Vector[GridLocation], acc: Int): Int =
        q match {
          case GridLocation(row: Int, col: Int) +: tail =>
            val target = map(row, col)
            val neighbors = map
              .neighbors(row, col)
              .filter(n => n.value > target && n.value != 9)
            val toAdd = neighbors
              .map(_.location)
              .filterNot(visited.contains)
            loop(q.tail ++ toAdd, visited ++ toAdd, acc + toAdd.size)
          case _ => acc
        }

      lowPoints.map(p => loop(Vector(p), Vector(p), 1)).sorted.reverse.take(3).product
    }

    private def lowPoints(map: Grid[Int]): List[GridLocation] = {
      val maybeLocations = for {
        row <- 0 until map.rows
        col <- 0 until map.columns
      } yield {
        val target = map(row, col)
        val neighbors = map.neighbors(row, col).map(_.value)
        if (neighbors.forall(_ > target)) Some(GridLocation(row, col)) else None
      }

      maybeLocations.flatten.toList
    }
  }

  override def run(args: List[String]): IO[ExitCode] = Runner.run(args)
}
