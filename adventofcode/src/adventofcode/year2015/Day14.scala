package adventofcode.year2015

import adventofcode.Day

case class Reindeer(name: String, speed: Int, speedSeconds: Int, restSeconds: Int)

object Reindeer {
  def from(line: String): Reindeer = {
    val pattern = "(.*) can fly (\\d+) km/s for (\\d+) seconds, but then must rest for (\\d+) seconds.".r
    val pattern(name, speed, speedSeconds, restSeconds) = line
    Reindeer(name, speed.toInt, speedSeconds.toInt, restSeconds.toInt)
  }
}

case class Position(reindeer: Reindeer, distance: Int, seconds: Int)

case class Day14Context(score: List[LazyList[Position]] => Int)

object Day14 extends Day[List[Reindeer], Day14Context, Int](2015, 14) {

  override def transformInput(lines: List[String]): List[Reindeer] =
    lines.map(Reindeer.from)

  override def partOneContext(): Option[Day14Context] =
    Some(Day14Context(partOneScoring(2503)))

  override def partTwoContext(): Option[Day14Context] =
    Some(Day14Context(partTwoScoring(2503)))

  override def process(input: List[Reindeer], context: Option[Day14Context]): Option[Int] = context.map { ctx =>
    def stepForward(pos: Position): Position = {
      val isResting = pos.seconds % (pos.reindeer.restSeconds + pos.reindeer.speedSeconds) >= pos.reindeer.speedSeconds
      val step = if (isResting) 0 else pos.reindeer.speed
      Position(pos.reindeer, pos.distance + step, pos.seconds + 1)
    }

    val lists = input.map(reindeer => LazyList.iterate(Position(reindeer, 0, 0))(stepForward))
    ctx.score(lists)
  }

  def partOneScoring(seconds: Int)(input: List[LazyList[Position]]): Int =
    input.map(_.apply(seconds)).map(_.distance).max

  def partTwoScoring(seconds: Int)(input: List[LazyList[Position]]): Int = {
    @annotation.tailrec
    def scoreEachSecond(score: Map[Reindeer, Int], remainingSeconds: List[Int]): Int = {
      remainingSeconds match {
        case Nil => score.values.max
        case head :: tail =>
          val results = input.map(_.apply(head))
          val maxDistance = results.sortWith(_.distance > _.distance).head.distance
          val winners = results
            .filter(_.distance == maxDistance)
            .map(p => (p.reindeer, score.getOrElse(p.reindeer, 0) + 1))
          scoreEachSecond(score ++ winners, tail)
      }
    }

    scoreEachSecond(Map.empty, Range(1, seconds + 1).toList)
  }
}
