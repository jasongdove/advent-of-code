package adventofcode.year2022

import adventofcode.{Day, Grid, GridLocation, WeightedGraph}
import cats.effect._

object Day12 extends IOApp {
  case class Input(grid: Grid[Int], start: GridLocation, finish: GridLocation)

  case object Input {
    def from(input: List[String]): Input = {
      val rows = input.length
      val cols = input.head.length
      var start = GridLocation(0, 0)
      var finish = GridLocation(0, 0)
      val data = for {
        (line, row) <- input.zipWithIndex
        (value, col) <- line.zipWithIndex
      } yield GridLocation(row, col) -> (value match {
        case 'S' => {
          start = GridLocation(row, col)
          1
        }
        case 'E' => {
          finish = GridLocation(row, col)
          26
        }
        case c => (c - 'a') + 1
      })

      Input(Grid(rows, cols, data.toMap), start, finish)
    }
  }

  def distancesFrom(
    input: Input,
    graph: WeightedGraph[GridLocation],
    start: GridLocation,
    up: Boolean
  ): Map[GridLocation, Int] = {
    val dist = scala.collection.mutable.Map.empty[GridLocation, Int]

    object MinOrder extends Ordering[(GridLocation, Int)] {
      def compare(x: (GridLocation, Int), y: (GridLocation, Int)): Int =
        y._2 compare x._2
    }

    val q = scala.collection.mutable.PriorityQueue.empty(MinOrder)

    graph.nodes.foreach { location =>
      if (location == start) {
        dist.addOne(location -> 0)
        q.enqueue(location -> 0)
      } else dist.addOne(location -> Int.MaxValue)
    }

    while (q.size > 0) {
      val current = q.dequeue()
      for (n <- graph.adj(current._1)) {
        val currentHeight = input.grid.data(current._1)
        val nextHeight = input.grid.data(n.value)
        val newDist =
          // traveling up (from start)
          if (up && currentHeight >= nextHeight) dist(current._1) + 1
          else if (up && currentHeight == nextHeight - 1) dist(current._1) + 1

          // traveling down (from finish)
          else if (!up && currentHeight <= nextHeight) dist(current._1) + 1
          else if (!up && currentHeight == nextHeight + 1) dist(current._1) + 1

          // can't travel here
          else Int.MaxValue
        if (newDist < dist(n.value)) {
          dist.update(n.value, newDist)
          q.enqueue(n.value -> newDist)
        }
      }
    }

    dist.toMap
  }

  case class Context(start: Input => GridLocation, up: Boolean, solve: (Input, Map[GridLocation, Int]) => Int) {
    def distances(input: Input): Map[GridLocation, Int] = {
      val graph = WeightedGraph.directedFrom(input.grid)
      distancesFrom(input, graph, start(input), up)
    }
  }

  object Runner extends Day[Input, Context, Int](2022, 12) {
    override def transformInput(lines: List[String]): Input =
      Input.from(lines)

    override def partOneContext(): Option[Context] =
      Some(Context(start = _.start, up = true, (input, distances) => distances(input.finish)))

    override def partTwoContext(): Option[Context] =
      Some(
        Context(
          start = _.finish,
          up = false,
          (input, distances) =>
            input.grid.data
              .filter { case (l: GridLocation, v: Int) => v == 1 } // 'a'
              .map { case (l: GridLocation, v: Int) => distances(l) }
              .min
        )
      )
    override def process(input: Input, context: Option[Context]): Option[Int] =
      context.map(ctx => { ctx.solve(input, ctx.distances(input)) })
  }

  override def run(args: List[String]): IO[ExitCode] = Runner.run(args)
}
