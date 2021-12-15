package adventofcode

case class Edge[A](from: A, to: A, weight: Int)
case class Target[A](value: A, weight: Int)

case class WeightedGraph[A](nodes: List[A], adj: Map[A, Set[Target[A]]]) {

  def hamiltonianWeights(start: A): Option[List[Int]] = weights(start, false)

  def tspWeights(start: A): Option[List[Int]] = weights(start, true)

  def areConnected(start: A, finish: A): Boolean = {
    if (start == finish) false
    else {
      @annotation.tailrec
      def loop(visited: List[A], work: List[A]): Boolean = {
        work match {
          case Nil => false
          case head :: next =>
            val nextVisited = visited :+ head
            adj.lift(head) match {
              case None      => loop(nextVisited, next)
              case Some(Nil) => loop(nextVisited, next)
              case Some(targets) =>
                if (targets.exists(_.value == finish)) true
                else {
                  val nextWork = next ++ targets.map(_.value).filterNot(nextVisited.contains)
                  loop(nextVisited, nextWork)
                }
            }
        }
      }

      loop(List(start), List(start))
    }
  }

  def countPaths(start: A, finish: A): Long = {
    val sorted = topoSort(start)

    val pathsToFinish = sorted.foldRight(Map[A, Long]((finish, 1))) { case (node, acc) =>
      adj.lift(node) match {
        case None => acc
        case Some(targets) =>
          acc ++ targets.foldLeft(acc) { case (a, target) =>
            val current = a.lift(node).getOrElse(0L)
            a.updated(node, current + a.lift(target.value).getOrElse(0L))
          }
      }
    }

    pathsToFinish(start)
  }

  def distancesFrom(start: A): Map[A, Long] = {
    val dist = scala.collection.mutable.Map.empty[A, Long]

    object MinOrder extends Ordering[(A, Long)] {
      def compare(x: (A, Long), y: (A, Long)): Int =
        y._2 compare x._2
    }

    val q = scala.collection.mutable.PriorityQueue.empty(MinOrder)

    nodes.foreach { location =>
      if (location == start) {
        dist.addOne(location -> 0)
        q.enqueue(location -> 0)
      } else dist.addOne(location -> Int.MaxValue)
    }

    while (q.size > 0) {
      val current = q.dequeue()
      for (n <- adj(current._1)) {
        val newDist = dist(current._1) + n.weight
        if (newDist < dist(n.value)) {
          dist.update(n.value, newDist)
          q.enqueue(n.value -> newDist)
        }
      }
    }

    dist.toMap
  }

  def shortestDistance(start: A, finish: A): Long =
    distancesFrom(start)(finish)

  private def topoSort(start: A): List[A] = {
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
          current +=: sorted
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

  def directedFrom(grid: Grid[Int]): WeightedGraph[GridLocation] = {
    @annotation.tailrec
    def buildEdges(q: Vector[GridValue[Int]], edges: Vector[Edge[GridLocation]]): List[Edge[GridLocation]] = {
      q match {
        case head +: tail =>
          val toAdd = grid
            .neighbors(head.location.row, head.location.col)
            .map(gv => Edge(head.location, gv.location, gv.value))
          buildEdges(tail, edges ++ toAdd)
        case _ => edges.toList
      }
    }

    val edges = buildEdges(grid.data.map(kv => GridValue(kv._1, kv._2)).toVector, Vector.empty)
    WeightedGraph.directedFrom(edges)
  }
}
