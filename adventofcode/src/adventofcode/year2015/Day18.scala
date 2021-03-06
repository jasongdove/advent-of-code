package adventofcode.year2015

import adventofcode.Day
import cats.effect._

object Day18 extends IOApp {
  case class AnimatedGrid(elements: Map[(Int, Int), Boolean]) {
    val size: Int = elements.keys.map(_._1).max

    def neighborsOn(row: Int, col: Int): Int = {
      val checks = List(
        (row - 1, col - 1),
        (row, col - 1),
        (row + 1, col - 1),
        (row - 1, col),
        (row + 1, col),
        (row - 1, col + 1),
        (row, col + 1),
        (row + 1, col + 1)
      )
      checks.count(c => elements.getOrElse(c, false))
    }
  }

  case class Context(steps: Int, transform: AnimatedGrid => AnimatedGrid)

  private def forceCornersOn(grid: AnimatedGrid): AnimatedGrid =
    AnimatedGrid(
      grid.elements ++ Map(
        ((0, 0), true),
        ((0, grid.size), true),
        ((grid.size, 0), true),
        ((grid.size, grid.size), true)
      )
    )

  object Runner extends Day[AnimatedGrid, Context, Int](2015, 18) {

    override def transformInput(lines: List[String]): AnimatedGrid = {
      val gridMap = lines.zipWithIndex.flatMap { case (line, row) =>
        line.zipWithIndex.map {
          case ('#', col) => ((row, col), true)
          case (_, col)   => ((row, col), false)
        }
      }.toMap
      AnimatedGrid(gridMap)
    }

    override def partOneContext(): Option[Context] =
      Some(Context(100, identity))

    override def partTwoContext(): Option[Context] =
      Some(Context(100, forceCornersOn))

    override def process(input: AnimatedGrid, context: Option[Context]): Option[Int] = {
      @annotation.tailrec
      def iteration(
        acc: Map[(Int, Int), Boolean],
        start: AnimatedGrid,
        remaining: List[((Int, Int), Boolean)]
      ): AnimatedGrid =
        remaining match {
          case Nil => AnimatedGrid(acc)
          case head :: tail =>
            val neighborsOn = (start.neighborsOn _).tupled(head._1)
            val turnOn = (head._2 && neighborsOn == 2 || neighborsOn == 3) || (!head._2 && neighborsOn == 3)
            iteration(acc.updated(head._1, turnOn), start, tail)
        }

      context.map { ctx =>
        val transformed = ctx.transform(input)
        val list = LazyList.iterate(transformed)(i => ctx.transform(iteration(i.elements, i, i.elements.toList)))
        list(ctx.steps).elements.count(_._2)
      }
    }
  }

  override def run(args: List[String]): IO[ExitCode] = Runner.run(args)
}
