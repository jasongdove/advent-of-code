package adventofcode.year2022

import adventofcode.{Day, Edge, WeightedGraph}
import cats.effect._
import scala.collection.mutable.PriorityQueue

object Day16 extends IOApp {
  case class Valve(id: String, flowRate: Int, targets: Set[String])

  case object Valve {
    def from(line: String): Valve = {
      val pattern = "Valve (.*) has flow rate=(\\d+); tunnels? leads? to valves? (.*)".r
      val pattern(from, weight, to) = line
      val targets = to.split(", ").toSet
      Valve(from, weight.toInt, targets)
    }
  }

  def allDistances(graph: WeightedGraph[String], valves: Map[String, Valve]): Map[String, Map[String, Long]] =
    graph.nodes.map(v => v -> graph.distancesFrom(v).filter { case (vid, _) => valves(vid).flowRate > 0 }).toMap

  def potential(
    allDistances: Map[String, Map[String, Long]],
    valves: Map[String, Valve],
    minutesRemaining: Int,
    valve: String,
    openValves: List[String]
  ): Int = allDistances(valve)
    .filterNot(d => openValves.contains(d._1))
    .filter(_._2 < minutesRemaining)
    .map { case (id, dist) =>
      math.max(minutesRemaining - dist.toInt, 0) * valves(id).flowRate
    }
    .sum

  def solve2(valves: Map[String, Valve], start: String, maxMinutes: Int): Long = {
    val edges = valves.values.flatMap(v => v.targets.map(t => Edge(v.id, t, 1))).toList
    val graph = WeightedGraph.directedFrom(edges)
    val distances = allDistances(graph, valves)
    val importantValves = valves.filter(_._2.flowRate > 0).map(_._1).toList

    case class Position(
      minutesRemaining1: Int,
      minutesRemaining2: Int,
      totalPressure: Int,
      openValves: List[String],
      valveId1: String,
      valveId2: String
    ) {
      val key: (Int, Int) = {
        var result = 0
        for (v <- openValves) {
          result = result | (1 << importantValves.indexOf(v))
        }
        (result, minutesRemaining1 + minutesRemaining2)
      }
    }

    def priorityOrder(p: Position) = p.totalPressure

    var max = 0L

    val queue = scala.collection.mutable
      .PriorityQueue[Position](Position(maxMinutes, maxMinutes, 0, List.empty, start, start))(
        Ordering.by(priorityOrder)
      )

    val best = scala.collection.mutable.Map.empty[(Int, Int), Int]

    while (queue.nonEmpty) {
      val p = queue.dequeue()
      if (p.totalPressure > max) {
        // println(
        //   s"Position(${p.minutesRemaining1} min, ${p.totalPressure}, ${p.openValves.sorted}, ${p.valveId1})"
        // )
        max = p.totalPressure
      }
      if (!best.contains(p.key) || best(p.key) < p.totalPressure) {
        best.update(p.key, p.totalPressure)
        val next1 = distances(p.valveId1)
          .filterNot(d => p.openValves.contains(d._1))
          .filter(_._2 < p.minutesRemaining1)
          .map { case (id, dist) =>
            (id, dist, (p.minutesRemaining1 - dist.toInt - 1) * valves(id).flowRate)
          }
          .filter(_._3 > 0)
          .map { case (target, dist, pressure) =>
            Position(
              p.minutesRemaining1 - dist.toInt - 1,
              p.minutesRemaining2,
              p.totalPressure + pressure,
              p.openValves :+ target,
              target,
              p.valveId2
            )
          }

        val next2 = distances(p.valveId2)
          .filterNot(d => p.openValves.contains(d._1))
          .filter(_._2 < p.minutesRemaining2)
          .map { case (id, dist) =>
            (id, dist, (p.minutesRemaining2 - dist.toInt - 1) * valves(id).flowRate)
          }
          .filter(_._3 > 0)
          .map { case (target, dist, pressure) =>
            Position(
              p.minutesRemaining1,
              p.minutesRemaining2 - dist.toInt - 1,
              p.totalPressure + pressure,
              p.openValves :+ target,
              p.valveId1,
              target
            )
          }

        queue.addAll(next1 ++ next2)
      }
    }

    max
  }

  def solve(valves: Map[String, Valve], start: String, maxMinutes: Int): Long = {
    val edges = valves.values.flatMap(v => v.targets.map(t => Edge(v.id, t, 1))).toList
    val graph = WeightedGraph.directedFrom(edges)
    val distances = allDistances(graph, valves)

    case class Position(minutesRemaining: Int, totalPressure: Int, openValves: List[String], valveId: String)

    def priorityOrder(p: Position) = p.totalPressure

    var max = 0L

    val visited = scala.collection.mutable.Set.empty[Position]

    val queue = scala.collection.mutable
      .PriorityQueue[Position](Position(maxMinutes, 0, List.empty, start))(Ordering.by(priorityOrder))

    while (queue.nonEmpty) {
      val p = queue.dequeue()
      if (p.totalPressure > max) {
        // println(
        //   s"Position(${p.minutesRemaining} min, ${p.totalPressure}, ${p.openValves.sorted}, ${p.valveId})"
        // )
        max = p.totalPressure
      }
      if (
        p.minutesRemaining > 0 && p.totalPressure + potential(
          distances,
          valves,
          p.minutesRemaining,
          p.valveId,
          p.openValves
        ) > max
      ) {
        val next = distances(p.valveId)
          .filterNot(d => p.openValves.contains(d._1))
          .filter(_._2 <= p.minutesRemaining)
          .map { case (id, dist) =>
            (id, dist, (p.minutesRemaining - dist.toInt - 1) * valves(id).flowRate)
          }
          .filter(_._3 > 0)
          .map { case (target, dist, pressure) =>
            Position(p.minutesRemaining - dist.toInt - 1, p.totalPressure + pressure, p.openValves :+ target, target)
          }

        queue.enqueue(next.toSeq: _*)
      }
    }

    max
  }

  case class Context(solve: List[Valve] => Long)

  object Runner extends Day[List[Valve], Context, Long](2022, 16) {
    override def transformInput(lines: List[String]): List[Valve] =
      lines.map(Valve.from)

    override def partOneContext(): Option[Context] =
      Some(Context(input => {
        val valves = input.map(v => (v.id, v)).toMap
        solve(valves, "AA", 30)
      }))

    override def partTwoContext(): Option[Context] =
      Some(Context(input => {
        val valves = input.map(v => (v.id, v)).toMap
        solve2(valves, "AA", 26)
      }))

    override def process(input: List[Valve], context: Option[Context]): Option[Long] =
      context.map(_.solve(input))
  }

  override def run(args: List[String]): IO[ExitCode] = Runner.run(args)
}
