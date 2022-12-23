package adventofcode.year2022

import adventofcode.Day
import cats.effect._

object Day19 extends IOApp {
  case class Blueprint(
    number: Int,
    oreRobotOreCost: Int,
    clayRobotOreCost: Int,
    obsidianRobotOreCost: Int,
    obsidianRobotClayCost: Int,
    geodeRobotOreCost: Int,
    geodeRobotObsidianCost: Int
  )

  case class State(
    minutesRemaining: Int,
    oreRobotCount: Int,
    clayRobotCount: Int,
    obsidianRobotCount: Int,
    geodeRobotCount: Int,
    oreCount: Int,
    clayCount: Int,
    obsidianCount: Int,
    geodeCount: Int,
    queuedOre: Int,
    queuedClay: Int,
    queuedObsidian: Int,
    queuedGeode: Int
  ) {
    val hashKey: Long =
      minutesRemaining | (geodeRobotCount << 7) | (obsidianRobotCount << 14) | (clayRobotCount << 21) | (oreRobotCount << 28) |
        (geodeCount << 35) | (obsidianCount << 42) | (clayCount << 49) | (oreCount << 56) | (queuedGeode << 58) | (queuedObsidian << 59) |
        (queuedClay << 60) | (queuedOre << 61)

    def geodeScore(b: Blueprint): Int =
      geodeCount * b.geodeRobotObsidianCost * b.obsidianRobotOreCost +
        obsidianCount * b.obsidianRobotOreCost +
        clayCount * b.clayRobotOreCost +
        oreCount

    def robotScore(b: Blueprint): Int =
      geodeRobotCount * b.geodeRobotObsidianCost * b.obsidianRobotOreCost +
        obsidianRobotCount * b.obsidianRobotOreCost +
        clayRobotCount * b.clayRobotOreCost +
        oreRobotCount

    def score(b: Blueprint): Int = (minutesRemaining + 1) * robotScore(b) + geodeScore(b)

    def collect(): State = {
      val nextGeode = geodeRobotCount + geodeCount
      val nextObsidian = obsidianRobotCount + obsidianCount
      val nextOre = oreRobotCount + oreCount
      val nextClay = clayRobotCount + clayCount
      copy(
        oreCount = nextOre,
        clayCount = nextClay,
        obsidianCount = nextObsidian,
        geodeCount = nextGeode
      )
    }

    def upperBoundSpend(b: Blueprint): Iterable[State] =
      Some(copy(queuedGeode = 1))

    def spend(b: Blueprint): Iterable[State] = {
      val nextStates =
        List(
          if (oreCount >= b.geodeRobotOreCost && obsidianCount >= b.geodeRobotObsidianCost) {
            Some(
              copy(
                oreCount = oreCount - b.geodeRobotOreCost,
                obsidianCount = obsidianCount - b.geodeRobotObsidianCost,
                queuedGeode = 1
              )
            )
          } else None,
          if (oreCount >= b.obsidianRobotOreCost && clayCount >= b.obsidianRobotClayCost) {
            Some(
              copy(
                oreCount = oreCount - b.obsidianRobotOreCost,
                clayCount = clayCount - b.obsidianRobotClayCost,
                queuedObsidian = 1
              )
            )
          } else None,
          if (oreCount >= b.clayRobotOreCost) {
            Some(
              copy(
                oreCount = oreCount - b.clayRobotOreCost,
                queuedClay = 1
              )
            )
          } else None,
          if (oreCount >= b.oreRobotOreCost) {
            Some(
              copy(
                oreCount = oreCount - b.oreRobotOreCost,
                queuedOre = 1
              )
            )
          } else None,
          Some(this)
        ).flatten

      nextStates
    }

    def start(minutes: Int): State = {
      // println(s"== Minute ${minutes - minutesRemaining + 1} ==")
      this
    }

    def end(): State = {
      copy(
        minutesRemaining = minutesRemaining - 1,
        oreRobotCount = oreRobotCount + queuedOre,
        clayRobotCount = clayRobotCount + queuedClay,
        obsidianRobotCount = obsidianRobotCount + queuedObsidian,
        geodeRobotCount = geodeRobotCount + queuedGeode,
        queuedOre = 0,
        queuedClay = 0,
        queuedObsidian = 0,
        queuedGeode = 0
      )
    }
  }

  case class Input(blueprints: List[Blueprint])

  case object Input {
    def from(lines: List[String]): Input = {
      val pattern =
        "Blueprint (\\d+): Each ore robot costs (\\d+) ore\\. Each clay robot costs (\\d+) ore\\. Each obsidian robot costs (\\d+) ore and (\\d+) clay\\. Each geode robot costs (\\d+) ore and (\\d+) obsidian\\.".r
      val blueprints = lines
        .filter(_.nonEmpty)
        .map(_ match {
          case pattern(number, oreCost, clayCost, obsidianCost1, obsidianCost2, geodeCost1, geodeCost2) =>
            Blueprint(
              number.toInt,
              oreCost.toInt,
              clayCost.toInt,
              obsidianCost1.toInt,
              obsidianCost2.toInt,
              geodeCost1.toInt,
              geodeCost2.toInt
            )
        })
      Input(blueprints)
    }
  }

  def upperBound(b: Blueprint, initialState: State): Int = {
    def priorityOrder(s: State): Int = s.geodeCount //* s.minutesRemaining

    val visited = scala.collection.mutable.Set.empty[State]
    val queue = scala.collection.mutable.PriorityQueue.empty[State](Ordering.by(priorityOrder))
    var best: Int = 0

    queue.enqueue(initialState)

    while (!queue.isEmpty) {
      var state = queue.dequeue()
      if (state.geodeCount > best) {
        best = state.geodeCount
      }
      if (state.minutesRemaining > 0) {
        queue.addAll(state.start(initialState.minutesRemaining).upperBoundSpend(b).map(_.collect().end()))
      }
    }

    best
  }

  def maxGeodes(blueprint: Blueprint, minutes: Int): Int = {
    def priorityOrder(s: State): Int = s.geodeCount + s.minutesRemaining

    val visited = scala.collection.mutable.HashSet.empty[Long]
    val queue = scala.collection.mutable.PriorityQueue.empty[State](Ordering.by(priorityOrder))
    var best: Int = 0

    queue.enqueue(State(minutes, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0))

    while (!queue.isEmpty) {
      var state = queue.dequeue()
      if (state.geodeCount > best) {
        best = state.geodeCount
      }
      if (!visited.contains(state.hashKey) && state.minutesRemaining > 0) {
        visited.add(state.hashKey)
        if (upperBound(blueprint, state) > best) {
          queue.addAll(state.start(minutes).spend(blueprint).map(_.collect().end()))
        }
      }
    }

    best
  }

  case class Context(solve: Input => Int)

  object Runner extends Day[Input, Context, Int](2022, 19) {
    override def transformInput(lines: List[String]): Input =
      Input.from(lines)

    override def partOneContext(): Option[Context] =
      Some(Context(input => {
        var sum = 0
        for (b <- input.blueprints) {
          val result = maxGeodes(b, 24) * b.number
          sum = sum + result
        }
        sum
      }))

    override def partTwoContext(): Option[Context] =
      Some(Context(input => {
        var product = 1
        for (b <- input.blueprints.take(3)) {
          val result = maxGeodes(b, 32)
          product = product * result
        }
        product
      }))

    override def process(input: Input, context: Option[Context]): Option[Int] =
      context.map(_.solve(input))
  }

  override def run(args: List[String]): IO[ExitCode] = Runner.run(args)
}
