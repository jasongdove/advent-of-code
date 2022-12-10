package adventofcode.year2022

import adventofcode.{Day, Grid, GridLocation}
import cats.effect._

object Day8 extends IOApp {

  case class Problem(grid: Grid[Int]) {
    def isVisible(row: Int, col: Int): Boolean = {
      if (row == 0 || col == 0 || row == grid.rows - 1 || col == grid.columns - 1) {
        true
      } else {
        val height = grid(row, col)
        val up = (0 to row - 1).map(grid(_, col))
        val down = (row + 1 to grid.rows - 1).map(grid(_, col))
        val left = (0 to col - 1).map(grid(row, _))
        val right = (col + 1 to grid.columns - 1).map(grid(row, _))
        up.forall(_ < height) || down.forall(_ < height) || left.forall(_ < height) || right.forall(_ < height)
      }
    }

    def scenicScore(row: Int, col: Int): Int = {
      val height = grid(row, col)
      val up = (row - 1 to 0 by -1).map(grid(_, col))
      val down = (row + 1 to grid.rows - 1).map(grid(_, col))
      val left = (col - 1 to 0 by -1).map(grid(row, _))
      val right = (col + 1 to grid.columns - 1).map(grid(row, _))

      val upTrees = up.indexWhere(_ >= height)
      val upScore = if (upTrees < 0) up.length else upTrees + 1

      val downTrees = down.indexWhere(_ >= height)
      val downScore = if (downTrees < 0) down.length else downTrees + 1

      val leftTrees = left.indexWhere(_ >= height)
      val leftScore = if (leftTrees < 0) left.length else leftTrees + 1

      val rightTrees = right.indexWhere(_ >= height)
      val rightScore = if (rightTrees < 0) right.length else rightTrees + 1

      upScore * downScore * leftScore * rightScore
    }
  }

  case class Context(solve: Problem => Int)

  object Runner extends Day[Problem, Context, Int](2022, 8) {
    override def transformInput(lines: List[String]): Problem = {
      val rows = lines.length
      val cols = lines.head.size
      var grid = Grid.fill(rows, cols)(0)
      for (row <- 0 to rows - 1) {
        val r = lines(row)
        for (col <- 0 to cols - 1) {
          grid = grid.updated(GridLocation(row, col), r(col).toString.toInt)
        }
      }
      Problem(grid)
    }

    override def partOneContext(): Option[Context] =
      Some(Context(l => l.grid.allLocations().count(loc => l.isVisible(loc.row, loc.col))))

    override def partTwoContext(): Option[Context] =
      Some(Context(l => l.grid.allLocations().map(loc => l.scenicScore(loc.row, loc.col)).max))

    override def process(input: Problem, context: Option[Context]): Option[Int] =
      context.map(_.solve(input))
  }

  override def run(args: List[String]): IO[ExitCode] = Runner.run(args)
}
