package adventofcode.year2015

import adventofcode.{Day, Edge, WeightedGraph}
import cats.effect._

object Day13 extends IOApp {
  case class Context(transformEdges: List[Edge[String]] => List[Edge[String]])

  private def edgesFrom(lines: List[String]): List[Edge[String]] = {
    case class Pair(left: String, right: String, weight: Int)
    val pattern = "(.*) would (gain|lose) (\\d+) happiness units by sitting next to (.*).".r
    val pairs = lines.map { line =>
      val pattern(from, gainLose, weight, to) = line
      val sign = if (gainLose == "gain") 1 else -1
      Pair(from, to, sign * weight.toInt)
    }

    @annotation.tailrec
    def buildEdges(edges: Map[(String, String), Int], pairs: List[Pair]): List[Edge[String]] = {
      pairs match {
        case Nil => edges.map(e => Edge(e._1._1, e._1._2, e._2)).toList
        case head :: tail =>
          val sorted = if (head.left > head.right) (head.left, head.right) else (head.right, head.left)
          val weight = edges.getOrElse(sorted, 0)
          buildEdges(edges.updated(sorted, head.weight + weight), tail)
      }
    }

    buildEdges(Map.empty, pairs)
  }

  private def addNeutralGuest(edges: List[Edge[String]]): List[Edge[String]] = {
    val allGuests = (edges.map(_.from) ++ edges.map(_.to)).toList.distinct
    val newEdges = allGuests.map(Edge("me", _, 0))
    edges ++ newEdges
  }

  object Runner extends Day[List[Edge[String]], Context, Long](2015, 13) {
    override def transformInput(lines: List[String]): List[Edge[String]] =
      edgesFrom(lines)

    override def partOneContext(): Option[Context] =
      Some(Context(identity))

    override def partTwoContext(): Option[Context] =
      Some(Context(addNeutralGuest))

    override def process(input: List[Edge[String]], context: Option[Context]): Option[Long] =
      context.map { ctx =>
        val graph = WeightedGraph.undirectedFrom(ctx.transformEdges(input))
        val weights = graph.adj.flatMap(a => graph.tspWeights(a._1)).flatten.toList
        weights.max.toLong
      }
  }

  override def run(args: List[String]): IO[ExitCode] = Runner.run(args)
}
