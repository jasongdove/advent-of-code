package adventofcode

case class Edge(from: String, to: String, weight: Int)
case class Target(name: String, weight: Int)

case class WeightedGraph(adj: Map[String, Set[Target]]) {
  def hamiltonianWeights(start: String): Option[List[Int]] = weights(start, false)

  def tspWeights(start: String): Option[List[Int]] = weights(start, true)

  private def weights(start: String, addFinalWeight: Boolean): Option[List[Int]] = {
    def loop(
      visited: List[String],
      current: String,
      weight: Int
    ): Option[List[Int]] = {
      if (adj.keySet.forall(visited.contains)) {
        val weightResult = if (addFinalWeight) {
          val weightToStart = adj(current).find(_.name == start).map(_.weight).getOrElse(0)
          weight + weightToStart
        } else weight

        Some(List(weightResult))
      } else {
        val adjWeights = adj(current)
          .filter(t => !visited.contains(t.name))
          .map(t => loop(t.name +: visited, t.name, weight + t.weight))
          .flatten
          .toList

        if (adjWeights.isEmpty) None else Some(adjWeights.flatten)
      }
    }
    loop(List(start), start, 0)
  }
}

object WeightedGraph {
  def from(edges: List[Edge]): WeightedGraph = {
    @annotation.tailrec
    def build(
      adj: Map[String, Set[Target]],
      edges: List[Edge]
    ): Map[String, Set[Target]] = {
      edges match {
        case Nil => adj
        case head :: tail =>
          val v = adj.getOrElse(head.from, Set.empty)
          val a = adj.updated(head.from, v + Target(head.to, head.weight))
          val v2 = a.getOrElse(head.to, Set.empty)
          val a2 = a.updated(head.to, v2 + Target(head.from, head.weight))
          build(a2, tail)
      }
    }
    WeightedGraph(build(Map.empty, edges))
  }
}
