// using java-opt -Xms64m
// using java-opt -Xmx1g

package adventofcode.year2021

import cats.effect._
import adventofcode._

object Day15 extends IOApp {

  case class Input(grid: Grid[Int])

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

  case class Context(transformGrid: Grid[Int] => Grid[Int])

  object Runner extends Day[Input, Context, Long](2021, 15) {
    override def transformInput(lines: List[String]): Input =
      Input.from(lines)

    override def partOneContext(): Option[Context] =
      Some(Context(identity))

    override def partTwoContext(): Option[Context] =
      Some(Context(expandGrid))

    override def process(input: Input, context: Option[Context]): Option[Long] =
      context.map(ctx => process(ctx.transformGrid(input.grid)))

    private def process(grid: Grid[Int]): Long = {
      val start = GridLocation(0, 0)
      val finish = GridLocation(grid.rows - 1, grid.columns - 1)

      WeightedGraph.directedFrom(grid).shortestDistance(start, finish)
    }

    private def expandGrid(grid: Grid[Int]): Grid[Int] = {
      val values = (for {
        x <- 0 until 5
        y <- 0 until 5
        r <- 0 until grid.rows
        c <- 0 until grid.columns
      } yield {
        val location = GridLocation(r + grid.rows * y, c + grid.rows * x)
        val value = ((grid(r, c) + x + y) - 1) % 9 + 1
        location -> value
      }).toMap

      Grid(grid.rows * 5, grid.columns * 5, values)
    }
  }

  override def run(args: List[String]): IO[ExitCode] = Runner.run(args)
}
