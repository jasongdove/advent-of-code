package adventofcode.year2015

import adventofcode.{Day, Edge, WeightedGraph}

case class Day9Context(aggregate: List[Int] => Option[Long])

object Day9 extends Day[WeightedGraph, Day9Context, Long](2015, 9) {

  override def transformInput(lines: List[String]): WeightedGraph =
    WeightedGraph.undirectedFrom(lines.map(edgeFrom))

  override def partOneContext(): Option[Day9Context] =
    Some(Day9Context(l => Some(l.min.toLong)))

  override def partTwoContext(): Option[Day9Context] =
    Some(Day9Context(l => Some(l.max.toLong)))

  override def process(input: WeightedGraph, context: Option[Day9Context]): Option[Long] = context.flatMap { ctx =>
    val weights = input.adj.flatMap(a => input.hamiltonianWeights(a._1)).flatten.toList
    ctx.aggregate(weights)
  }

  private def edgeFrom(line: String): Edge = {
    val pattern = "(.*) to (.*) = (.*)".r
    val pattern(from, to, weight) = line
    Edge(from, to, weight.toInt)
  }
}
