package adventofcode.year2015

import adventofcode.{Day, Edge, WeightedGraph}

case class Day13Context(transformEdges: List[Edge] => List[Edge])

object Day13 extends Day[List[Edge], Day13Context, Long](2015, 13) {
  override def transformInput(lines: List[String]): List[Edge] =
    edgesFrom(lines)

  override def partOneContext(): Option[Day13Context] =
    Some(Day13Context(identity))

  override def partTwoContext(): Option[Day13Context] =
    Some(Day13Context(addNeutralGuest))

  override def process(input: List[Edge], context: Option[Day13Context]): Option[Long] =
    context.map { ctx =>
      val graph = WeightedGraph.from(ctx.transformEdges(input))
      val weights = graph.adj.flatMap(a => graph.tspWeights(a._1)).flatten.toList
      weights.max.toLong
    }

  private def edgesFrom(lines: List[String]): List[Edge] = {
    case class Pair(left: String, right: String, weight: Int)
    val pattern = "(.*) would (gain|lose) (\\d+) happiness units by sitting next to (.*).".r
    val pairs = lines.map { line =>
      val pattern(from, gainLose, weight, to) = line
      val sign = if (gainLose == "gain") 1 else -1
      Pair(from, to, sign * weight.toInt)
    }

    @annotation.tailrec
    def buildEdges(edges: Map[(String, String), Int], pairs: List[Pair]): List[Edge] = {
      pairs match {
        case Nil => edges.map(e => Edge(e._1._1, e._1._2, e._2)).toList
        case head :: tail => {
          val sorted = if (head.left > head.right) (head.left, head.right) else (head.right, head.left)
          val weight = edges.get(sorted).getOrElse(0)
          buildEdges(edges.updated(sorted, head.weight + weight), tail)
        }
      }
    }

    buildEdges(Map.empty, pairs)
  }

  private def addNeutralGuest(edges: List[Edge]): List[Edge] = {
    val allGuests = (edges.map(_.from) ++ edges.map(_.to)).toList.distinct
    val newEdges = allGuests.map(Edge("me", _, 0))
    edges ++ newEdges
  }
}
