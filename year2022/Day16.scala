package adventofcode.year2022

import adventofcode.{Day, Edge}
import cats.effect._
import adventofcode.WeightedGraph
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

  def solve(valves: Map[String, Valve], start: String): Long = {
    case class Position(minutesRemaining: Int, totalPressure: Int, openValves: List[String], valveId: String)

    println(valves.mkString("\n"))

    def potentialPressure(totalPressure: Int, minutesRemaining: Int, openValves: List[String]) =
      totalPressure + valves.filterNot(openValves.contains).map(v => v._2.flowRate * minutesRemaining).sum

    def priorityOrder(p: Position) = p.totalPressure

    var max = 0;

    val queue = scala.collection.mutable
      .PriorityQueue[Position](Position(30, 0, List.empty, start))(Ordering.by(priorityOrder))

    var count = 0;
    while (queue.nonEmpty && count < 10_00_000_000) {
      if (count % 1_000_000 == 0) {
        println(s"count: ${count}, max: ${max}")
      }
      count = count + 1
      val Position(minutesRemaining, totalPressure, openValves, currentValveId) = queue.dequeue()
      if (totalPressure > max) {
        max = totalPressure
      }
      if (minutesRemaining == 0) {
        // do nothing
        // if potential pressure <= max, continue
      } else if (potentialPressure(totalPressure, minutesRemaining, openValves) > max) {
        // println(s"Position(${minutesRemaining} min, ${totalPressure}, ${openValves}, ${currentValveId}) max ${max}")
        val valve = valves(currentValveId)

        val next = if (openValves.contains(currentValveId)) {
          valve.targets.map(target =>
            Position(
              minutesRemaining - 1,
              totalPressure,
              openValves,
              target
            )
          )
        } else {
          valve.targets.flatMap(target =>
            Seq(
              Position(
                minutesRemaining - 1,
                totalPressure,
                openValves,
                target
              ),
              Position(
                math.max(minutesRemaining - 2, 0),
                totalPressure + (minutesRemaining - 1) * valve.flowRate,
                openValves :+ valve.id,
                target
              )
            )
          )
        }

        // println(s"${next.mkString("\n")}")

        queue.enqueue(next.toSeq: _*)
      }
    }
    // }

    // max
    0
  }

  case class Context(solve: List[Valve] => Long)

  object Runner extends Day[List[Valve], Context, Long](2022, 16) {
    override def transformInput(lines: List[String]): List[Valve] =
      lines.map(Valve.from)

    override def partOneContext(): Option[Context] =
      Some(Context(input => {
        val _ = input
        // println(input.mkString("\n"))
        val valves = input.map(v => (v.id, v)).toMap
        println(solve(valves, input.head.id))
        0
      }))

    override def partTwoContext(): Option[Context] =
      Some(Context(input => {
        val _ = input
        0
      }))

    override def process(input: List[Valve], context: Option[Context]): Option[Long] =
      context.map(_.solve(input))
  }

  override def run(args: List[String]): IO[ExitCode] = Runner.run(args)
}
