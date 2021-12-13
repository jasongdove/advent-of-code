package adventofcode.year2021

import cats.effect._
import adventofcode._

object Day13 extends IOApp {

  case class FoldInstruction(direction: String, at: Int) {
    def perform(grid: Grid[Boolean]): Grid[Boolean] =
      direction match {
        case "x" =>
          val newRows = grid.rows
          val newCols = at
          val data = grid.data.filter { case (location, _) => location.col <= newCols }
          val transformed = grid.data
            .filter { case (location, _) => location.col > newCols && location.col < grid.columns }
            .map { case (location, dot) =>
              val newLocation = location.copy(col = grid.columns - location.col - 1)
              newLocation -> dot
            }
          val newData = transformed.foldLeft(data) { case (acc, (location, dot)) =>
            acc.updated(location, dot || acc(location))
          }
          Grid(newRows, newCols, newData)
        case "y" =>
          val newRows = at
          val newCols = grid.columns
          val data = grid.data.filter { case (location, _) => location.row <= newRows }
          val transformed = grid.data
            .filter { case (location, _) => location.row > newRows && location.row < grid.rows }
            .map { case (location, dot) =>
              val newLocation = location.copy(row = grid.rows - location.row - 1)
              newLocation -> dot
            }
          val newData = transformed.foldLeft(data) { case (acc, (location, dot)) =>
            acc.updated(location, dot || acc(location))
          }
          Grid(newRows, newCols, newData)
      }
  }

  case class Input(grid: Grid[Boolean], foldInstructions: List[FoldInstruction])

  case object Input {
    private val coordinatePattern = "(\\d+),(\\d+)".r
    private val foldPattern = "fold along ([xy])=(\\d+)".r

    def from(input: List[String]): Input = {
      val coordinates = input.flatMap { line =>
        line match {
          case coordinatePattern(x, y) => Some(GridLocation(y.toInt, x.toInt))
          case _                       => None
        }
      }

      val folds = input.flatMap { line =>
        line match {
          case foldPattern(direction, at) => Some(FoldInstruction(direction, at.toInt))
          case _                          => None
        }
      }

      val empty = Grid.fill(coordinates.map(_.row).sorted.max + 1, coordinates.map(_.col).sorted.max + 1)(false)
      Input(coordinates.foldLeft(empty)((acc, coordinate) => acc.updated(coordinate, true)), folds)
    }
  }

  case class Context(process: Input => Long)

  object Runner extends Day[Input, Context, Long](2021, 13) {
    override def transformInput(lines: List[String]): Input =
      Input.from(lines)

    override def partOneContext(): Option[Context] =
      Some(Context(processPartOne))

    override def partTwoContext(): Option[Context] =
      Some(Context(processPartTwo))

    override def process(input: Input, context: Option[Context]): Option[Long] =
      context.map(_.process(input))

    private def processPartOne(input: Input): Long = {
      val first = input.foldInstructions.head.perform(input.grid)
      first.data.count { case (_, dot) => dot }.toLong
    }

    private def processPartTwo(input: Input): Long = {
      val result = input.foldInstructions.foldLeft(input.grid) { case (grid, instruction) =>
        instruction.perform(grid)
      }

      printGrid(result)
      0L
    }

    private def printGrid(grid: Grid[Boolean]) = {
      for (r <- 0 until grid.rows) {
        for (c <- 0 until grid.columns) {
          print(if (grid.data(GridLocation(r, c))) '#' else '.')
        }
        println()
      }
      println()
    }
  }

  override def run(args: List[String]): IO[ExitCode] = Runner.run(args)
}
