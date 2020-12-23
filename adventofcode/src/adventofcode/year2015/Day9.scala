package adventofcode.year2015

import adventofcode.{Day, Edge, WeightedGraph}
import cats.effect._

object Day9 extends IOApp {
  case class Context(aggregate: List[Int] => Option[Long])

  private def edgeFrom(line: String): Edge[String] = {
    val pattern = "(.*) to (.*) = (.*)".r
    val pattern(from, to, weight) = line
    Edge(from, to, weight.toInt)
  }

  object Runner extends Day[WeightedGraph[String], Context, Long](2015, 9) {

    override def transformInput(lines: List[String]): WeightedGraph[String] =
      WeightedGraph.undirectedFrom(lines.map(edgeFrom))

    override def partOneContext(): Option[Context] =
      Some(Context(l => Some(l.min.toLong)))

    override def partTwoContext(): Option[Context] =
      Some(Context(l => Some(l.max.toLong)))

    override def process(input: WeightedGraph[String], context: Option[Context]): Option[Long] = context.flatMap {
      ctx =>
        val weights = input.adj.flatMap(a => input.hamiltonianWeights(a._1)).flatten.toList
        ctx.aggregate(weights)
    }
  }

  override def run(args: List[String]): IO[ExitCode] = Runner.run(args)
}
