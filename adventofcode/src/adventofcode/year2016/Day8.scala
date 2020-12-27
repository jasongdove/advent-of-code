package adventofcode.year2016

import adventofcode.{Day, Grid, GridLocation}
import cats.effect._

object Day8 extends IOApp {
  sealed trait Instruction {
    def apply(grid: Grid[Boolean]): Grid[Boolean]
  }

  object Instruction {
    case class Rect(width: Int, height: Int) extends Instruction {
      def apply(grid: Grid[Boolean]): Grid[Boolean] = {
        val locations = for {
          r <- 0 until height
          c <- 0 until width
        } yield GridLocation(r, c)
        locations.foldLeft(grid) { case (g, l) => g.updated(l, true) }
      }
    }

    case class RotateColumn(column: Int, by: Int) extends Instruction {
      def apply(grid: Grid[Boolean]): Grid[Boolean] = {
        val locations = for {
          r <- 0 until grid.rows
        } yield (GridLocation((r + by) % grid.rows, column) -> grid(r, column))
        locations.foldLeft(grid) { case (g, l) => g.updated(l._1, l._2) }
      }
    }

    case class RotateRow(row: Int, by: Int) extends Instruction {
      def apply(grid: Grid[Boolean]): Grid[Boolean] = {
        val locations = for {
          c <- 0 until grid.columns
        } yield (GridLocation(row, (c + by) % grid.columns) -> grid(row, c))
        locations.foldLeft(grid) { case (g, l) => g.updated(l._1, l._2) }
      }
    }

    private val rectPattern = "rect (\\d+)x(\\d+)".r
    private val rotateColPattern = "rotate column x=(\\d+) by (\\d+)".r
    private val rotateRowPattern = "rotate row y=(\\d+) by (\\d+)".r
    def from(line: String): Instruction = {
      line match {
        case rectPattern(width, height)   => Rect(width.toInt, height.toInt)
        case rotateColPattern(column, by) => RotateColumn(column.toInt, by.toInt)
        case rotateRowPattern(row, by)    => RotateRow(row.toInt, by.toInt)
      }
    }
  }

  implicit class BooleanGrid(grid: Grid[Boolean]) {
    def debug() = {
      for (r <- (0 to grid.rows - 1)) {
        for (c <- (0 to grid.columns - 1))
          print(grid(r, c) match {
            case true  => "#"
            case false => "."
          })
        println()
      }
      println()
    }
  }

  case class Context(debug: Boolean)

  object Runner extends Day[List[Instruction], Context, Int](2016, 8) {
    override def transformInput(lines: List[String]): List[Instruction] =
      lines.map(Instruction.from)

    override def partOneContext(): Option[Context] =
      Some(Context(debug = false))

    override def partTwoContext(): Option[Context] =
      Some(Context(debug = true))

    override def process(input: List[Instruction], context: Option[Context]): Option[Int] =
      context.map { ctx =>
        val grid = Grid.fill(6, 50)(false)
        val result = input.foldLeft(grid) { case (g, i) => i.apply(g) }
        if (ctx.debug) result.debug()
        result.data.count(_._2)
      }
  }

  override def run(args: List[String]): IO[ExitCode] = Runner.run(args)
}
