package adventofcode.year2015

import adventofcode.Day

case class Edge(from: String, to: String, distance: Int)

object Edge {
  def from(line: String): Edge = {
    val pattern = "(.*) to (.*) = (.*)".r
    val pattern(from, to, distance) = line
    Edge(from, to, distance.toInt)
  }
}

case class Target(name: String, distance: Int)

case class Graph(adj: Map[String, Set[Target]])

object Graph {
  def from(edges: List[Edge]): Graph = {
    @annotation.tailrec
    def build(
      adj: Map[String, Set[Target]],
      edges: List[Edge]
    ): Map[String, Set[Target]] = {
      edges match {
        case Nil => adj
        case head :: tail =>
          val v = adj.getOrElse(head.from, Set.empty)
          val a = adj.updated(head.from, v + Target(head.to, head.distance))
          val v2 = a.getOrElse(head.to, Set.empty)
          val a2 = a.updated(head.to, v2 + Target(head.from, head.distance))
          build(a2, tail)
      }
    }
    Graph(build(Map.empty, edges))
  }
}

case class Day9Context(aggregate: List[Int] => Option[Long])

object Day9 extends Day[Graph, Day9Context](2015, 9) {

  override def transformInput(lines: List[String]): Graph =
    Graph.from(lines.map(Edge.from))

  override def partOneContext(): Option[Day9Context] =
    Some(Day9Context(l => Some(l.min.toLong)))

  override def partTwoContext(): Option[Day9Context] =
    Some(Day9Context(l => Some(l.max.toLong)))

  override def process(input: Graph, context: Option[Day9Context]): Option[Long] = context.flatMap { ctx =>
    val distances = input.adj.flatMap(a => hamiltonianDistances(input, a._1)).flatten.toList
    ctx.aggregate(distances)
  }

  private def hamiltonianDistances(graph: Graph, start: String): Option[List[Int]] = {
    def loop(
      visited: List[String],
      current: String,
      distance: Int
    ): Option[List[Int]] = {
      if (graph.adj.keySet.forall(visited.contains)) {
        Some(List(distance))
      } else {
        val adjDistances = graph
          .adj(current)
          .filter(t => !visited.contains(t.name))
          .map(t => loop(t.name +: visited, t.name, distance + t.distance))
          .flatten
          .toList

        if (adjDistances.isEmpty) None else Some(adjDistances.flatten)
      }
    }
    loop(List(start), start, 0)
  }
}
