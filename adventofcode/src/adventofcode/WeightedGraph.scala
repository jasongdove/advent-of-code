package adventofcode

case class Edge(from: String, to: String, weight: Int)
case class Target(name: String, weight: Int)

case class WeightedGraph(nodes: List[String], adj: Map[String, Set[Target]]) {

  def hamiltonianWeights(start: String): Option[List[Int]] = weights(start, false)

  def tspWeights(start: String): Option[List[Int]] = weights(start, true)

  def areConnected(start: String, finish: String): Boolean = {
    if (start == finish) false
    else {
      def loop(current: String): Boolean = {
        if (!adj.keySet.contains(current) || adj(current).isEmpty) false
        else if (adj(current).exists(_.name == finish)) true
        else adj(current).exists(t => loop(t.name))
      }

      loop(start)
    }
  }

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

  def undirectedFrom(edges: List[Edge]): WeightedGraph = {
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

    val adjList = build(Map.empty, edges)
    val nodes = adjList.keys ++ adjList.flatMap(_._2.map(_.name))
    WeightedGraph(nodes.toList, adjList)
  }

  def directedFrom(edges: List[Edge]): WeightedGraph = {
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
          build(a, tail)
      }
    }

    val adjList = build(Map.empty, edges)
    val nodes = adjList.keys ++ adjList.flatMap(_._2.map(_.name))
    WeightedGraph(nodes.toList, adjList)
  }
}
