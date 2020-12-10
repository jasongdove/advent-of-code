package adventofcode

case class Edge[A](from: A, to: A, weight: Int)
case class Target[A](value: A, weight: Int)

case class WeightedGraph[A](nodes: List[A], adj: Map[A, Set[Target[A]]]) {

  def hamiltonianWeights(start: A): Option[List[Int]] = weights(start, false)

  def tspWeights(start: A): Option[List[Int]] = weights(start, true)

  def areConnected(start: A, finish: A): Boolean = {
    if (start == finish) false
    else {
      def loop(current: A): Boolean = {
        if (!adj.keySet.contains(current) || adj(current).isEmpty) false
        else if (adj(current).exists(_.value == finish)) true
        else adj(current).exists(t => loop(t.value))
      }

      loop(start)
    }
  }

  def countPaths(start: A, finish: A): Long = {
    val sorted = topoSort(start)

    val pathsToFinish = sorted.foldRight(Map[A, Long]((finish, 1))) { case (node, acc) =>
      adj.lift(node) match {
        case None => acc
        case Some(targets) =>
          acc ++ targets.foldLeft(acc) { case (a, target) =>
            val current = if (a.contains(node)) a(node) else 0
            a.updated(node, current + a(target.value))
          }
      }
    }

    pathsToFinish(start)
  }

  private def topoSort(start: A): List[A] = {
    // TODO: maybe use accumulators
    val visited = scala.collection.mutable.Set[A]()
    val sorted = scala.collection.mutable.ListBuffer[A]()

    def loop(current: A): Unit = {
      visited.add(current)
      adj.lift(current) match {
        case None => ()
        case Some(targets) =>
          targets.foreach { target =>
            if (!visited.contains(target.value)) loop(target.value)
          }
          sorted.insert(0, current)
      }
    }

    loop(start)
    sorted.toList
  }

  private def weights(start: A, addFinalWeight: Boolean): Option[List[Int]] = {
    def loop(
      visited: List[A],
      current: A,
      weight: Int
    ): Option[List[Int]] = {
      if (adj.keySet.forall(visited.contains)) {
        val weightResult = if (addFinalWeight) {
          val weightToStart = adj(current).find(_.value == start).map(_.weight).getOrElse(0)
          weight + weightToStart
        } else weight

        Some(List(weightResult))
      } else {
        val adjWeights = adj(current)
          .filter(t => !visited.contains(t.value))
          .map(t => loop(t.value +: visited, t.value, weight + t.weight))
          .flatten
          .toList

        if (adjWeights.isEmpty) None else Some(adjWeights.flatten)
      }
    }
    loop(List(start), start, 0)
  }
}

object WeightedGraph {

  def undirectedFrom[A](edges: List[Edge[A]]): WeightedGraph[A] = {
    @annotation.tailrec
    def build(
      adj: Map[A, Set[Target[A]]],
      edges: List[Edge[A]]
    ): Map[A, Set[Target[A]]] = {
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
    val nodes = adjList.keys ++ adjList.flatMap(_._2.map(_.value))
    WeightedGraph(nodes.toList, adjList)
  }

  def directedFrom[A](edges: List[Edge[A]]): WeightedGraph[A] = {
    @annotation.tailrec
    def build(
      adj: Map[A, Set[Target[A]]],
      edges: List[Edge[A]]
    ): Map[A, Set[Target[A]]] = {
      edges match {
        case Nil => adj
        case head :: tail =>
          val v = adj.getOrElse(head.from, Set.empty)
          val a = adj.updated(head.from, v + Target(head.to, head.weight))
          build(a, tail)
      }
    }

    val adjList = build(Map.empty, edges)
    val nodes = adjList.keys ++ adjList.flatMap(_._2.map(_.value))
    WeightedGraph(nodes.toList, adjList)
  }
}
