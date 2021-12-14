package adventofcode.year2021

import cats.effect._
import adventofcode._
import adventofcode.utils._

object Day12 extends IOApp {

  case class Input(graph: WeightedGraph[String])

  case object Input {
    def from(input: List[String]): Input = {
      val edges = input.map { line =>
        val sides = line.split('-')
        Edge(sides(0), sides(1), 0)
      }
      Input(WeightedGraph.undirectedFrom(edges))
    }
  }

  case class Context(filter: (List[String], Vector[String]) => Boolean)

  object Runner extends Day[Input, Context, Long](2021, 12) {
    override def transformInput(lines: List[String]): Input =
      Input.from(lines)

    override def partOneContext(): Option[Context] =
      Some(Context(noDoubleSmallCaves))

    override def partTwoContext(): Option[Context] =
      Some(Context(doublesNoMoreThanOne))

    override def process(input: Input, context: Option[Context]): Option[Long] =
      context.map(ctx => countPaths(input.graph, "start", "end", ctx.filter))

    def countPaths(
      graph: WeightedGraph[String],
      start: String,
      finish: String,
      filter: (List[String], Vector[String]) => Boolean
    ): Long = {
      case class Path(path: Vector[String])

      @annotation.tailrec
      def pathsToFinish(q: Vector[Path], count: Int): Int =
        q match {
          case head +: tail =>
            val next = graph
              .adj(head.path.last)
              .filter(target => target.value != start)
              .map(t => Path(head.path :+ t.value))
              .filter(t => t.path.last.toUpperCase() == t.path.last || filter(graph.nodes, t.path))
            val unfinished = next.filter(_.path.last != finish)
            val finished = next.filter(_.path.last == finish)
            pathsToFinish(tail ++ unfinished, count + finished.size)
          case _ => count
        }

      pathsToFinish(Vector(Path(Vector(start))), 0)
    }

    private def noDoubleSmallCaves(nodes: List[String], path: Vector[String]): Boolean = {
      val lower = nodes.filter(n => n.toLowerCase() == n)

      val sizes = path
        .filter(lower.contains)
        .frequency

      sizes.forall(_._2 == 1)
    }

    private def doublesNoMoreThanOne(nodes: List[String], path: Vector[String]): Boolean = {
      val lower = nodes.filter(n => n.toLowerCase() == n)

      val sizes = path
        .filter(lower.contains)
        .frequency

      sizes.forall(_._2 <= 2) && sizes.count(_._2 > 1) <= 1
    }
  }

  override def run(args: List[String]): IO[ExitCode] = Runner.run(args)
}
