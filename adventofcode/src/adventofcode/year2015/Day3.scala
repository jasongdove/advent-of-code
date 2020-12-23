package adventofcode.year2015

import adventofcode.Day
import cats.effect._

object Day3 extends IOApp {
  sealed trait Direction

  case object North extends Direction
  case object East extends Direction
  case object West extends Direction
  case object South extends Direction

  object Direction {
    def from(char: Char): Direction =
      char match {
        case '<' => West
        case '>' => East
        case '^' => North
        case 'v' => South
      }
  }

  case class Context(process: List[Direction] => Option[Long])

  private def processPartOne(input: List[Direction]): Option[Long] =
    Some(housesWithPresents(input).size.toLong)

  private def processPartTwo(input: List[Direction]): Option[Long] = {
    val santa = input.zipWithIndex.collect {
      case (x, i) if i % 2 == 0 => x
    }
    val roboSanta = input.zipWithIndex.collect {
      case (x, i) if i % 2 == 1 => x
    }

    val combined = housesWithPresents(santa) ++ housesWithPresents(roboSanta)
    Some(combined.size.toLong)
  }

  private def housesWithPresents(input: List[Direction]): Set[(Int, Int)] = {
    @annotation.tailrec
    def loop(acc: Set[(Int, Int)], x: Int, y: Int, directions: List[Direction]): Set[(Int, Int)] = {
      if (directions.isEmpty) acc
      else {
        val (nextx, nexty) = directions.head match {
          case East  => (x + 1, y)
          case North => (x, y - 1)
          case South => (x, y + 1)
          case West  => (x - 1, y)
        }

        if (acc.contains((nextx, nexty)))
          loop(acc, nextx, nexty, directions.tail)
        else
          loop(acc + ((nextx, nexty)), nextx, nexty, directions.tail)
      }
    }

    loop(Set((0, 0)), 0, 0, input)
  }

  object Runner extends Day[List[Direction], Context, Long](2015, 3) {
    override def transformInput(lines: List[String]): List[Direction] =
      lines.mkString.trim.map(Direction.from).toList

    override def partOneContext(): Option[Context] =
      Some(Context(processPartOne))

    override def partTwoContext(): Option[Context] =
      Some(Context(processPartTwo))

    override def process(input: List[Direction], context: Option[Context]): Option[Long] =
      context.flatMap(_.process(input))
  }

  override def run(args: List[String]): IO[ExitCode] = Runner.run(args)
}
