package adventofcode.year2016

import adventofcode.{Day, Grid, GridLocation}

sealed trait Day8Instruction {
  def apply(grid: Grid[Boolean]): Grid[Boolean]
}

object Day8Instruction {
  case class Rect(width: Int, height: Int) extends Day8Instruction {
    def apply(grid: Grid[Boolean]): Grid[Boolean] = {
      val locations = for {
        r <- (0 to height - 1)
        c <- (0 to width - 1)
      } yield GridLocation(r, c)
      locations.foldLeft(grid) { case (g, l) => g.updated(l, true) }
    }
  }

  case class RotateColumn(column: Int, by: Int) extends Day8Instruction {
    def apply(grid: Grid[Boolean]): Grid[Boolean] = {
      val locations = for {
        r <- (0 to grid.rows - 1)
      } yield (GridLocation((r + by) % grid.rows, column) -> grid(r, column))
      locations.foldLeft(grid) { case (g, l) => g.updated(l._1, l._2) }
    }
  }

  case class RotateRow(row: Int, by: Int) extends Day8Instruction {
    def apply(grid: Grid[Boolean]): Grid[Boolean] = {
      val locations = for {
        c <- (0 to grid.columns - 1)
      } yield (GridLocation(row, (c + by) % grid.columns) -> grid(row, c))
      locations.foldLeft(grid) { case (g, l) => g.updated(l._1, l._2) }
    }
  }

  private val rectPattern = "rect (\\d+)x(\\d+)".r
  private val rotateColPattern = "rotate column x=(\\d+) by (\\d+)".r
  private val rotateRowPattern = "rotate row y=(\\d+) by (\\d+)".r
  def from(line: String): Day8Instruction = {
    line match {
      case rectPattern(width, height)   => Rect(width.toInt, height.toInt)
      case rotateColPattern(column, by) => RotateColumn(column.toInt, by.toInt)
      case rotateRowPattern(row, by)    => RotateRow(row.toInt, by.toInt)
    }
  }
}

case class Day8Context()

object Day8 extends Day[List[Day8Instruction], Day8Context, Int](2016, 8) {
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

  override def transformInput(lines: List[String]): List[Day8Instruction] =
    lines.map(Day8Instruction.from)

  override def partOneContext(): Option[Day8Context] =
    Some(Day8Context())

  override def partTwoContext(): Option[Day8Context] =
    Some(Day8Context())

  override def process(input: List[Day8Instruction], context: Option[Day8Context]): Option[Int] = {
    val grid = Grid.fill(6, 50)(false)
    val result = input.foldLeft(grid) { case (g, i) => i.apply(g) }
    result.debug()
    Some(result.data.count(_._2))
  }
}
