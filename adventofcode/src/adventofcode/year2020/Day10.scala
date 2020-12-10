package adventofcode.year2020

import adventofcode.{Day, Edge, WeightedGraph}

case class Day10Adapter(rating: Int)
case class Day10Context(process: WeightedGraph[Day10Adapter] => Long)

object Day10 extends Day[WeightedGraph[Day10Adapter], Day10Context, Long](2020, 10) {
  override def transformInput(lines: List[String]): WeightedGraph[Day10Adapter] = {
    WeightedGraph.directedFrom(edgesFrom(lines.map(_.toInt)))
  }

  override def partOneContext(): Option[Day10Context] =
    Some(Day10Context(processPartOne))

  override def partTwoContext(): Option[Day10Context] =
    Some(Day10Context(processPartTwo))

  override def process(input: WeightedGraph[Day10Adapter], context: Option[Day10Context]): Option[Long] =
    context.map(_.process(input))

  private def processPartOne(graph: WeightedGraph[Day10Adapter]): Long = {
    val ratings = graph.nodes.map(_.rating).sorted
    val diffs = ratings.sliding(2).map(l => l(1) - l(0)).toList
    diffs.count(_ == 1).toLong * diffs.count(_ == 3)
  }

  private def processPartTwo(graph: WeightedGraph[Day10Adapter]): Long = {
    val sorted = graph.nodes.sortBy(_.rating)
    graph.countPaths(sorted.head, sorted.last)
  }

  private def edgesFrom(lines: List[Int]): List[Edge[Day10Adapter]] = {
    def loop(acc: List[Edge[Day10Adapter]], remaining: List[Int]): List[Edge[Day10Adapter]] = {
      remaining match {
        case Nil => acc
        case head :: next =>
          val adapter = Day10Adapter(head)
          val allOptions = next.filter(r => r >= adapter.rating + 1 && r <= adapter.rating + 3)
          val newEdges = allOptions.map { o =>
            val a = Day10Adapter(o)
            Edge(adapter, a, a.rating - adapter.rating)
          }
          loop(acc ++ newEdges, next)
      }
    }

    val allRatings = List(0, lines.max + 3) ++ lines
    loop(List.empty, allRatings.sorted)
  }
}
