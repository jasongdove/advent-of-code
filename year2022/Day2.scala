package adventofcode.year2022

import adventofcode.Day
import cats.effect._

object Day1 extends IOApp {

  case class Move(move: String) {
    def score(other: Move): Int =
      (other.move, move) match {
        case ("A", "X") => 1 + 3
        case ("B", "X") => 1 + 0
        case ("C", "X") => 1 + 6
        case ("A", "Y") => 2 + 6
        case ("B", "Y") => 2 + 3
        case ("C", "Y") => 2 + 0
        case ("A", "Z") => 3 + 0
        case ("B", "Z") => 3 + 6
        case ("C", "Z") => 3 + 3
      }

    def score2(other: Move): Int =
      (other.move, move) match {
        case ("A", "X") => 3 + 0
        case ("B", "X") => 1 + 0
        case ("C", "X") => 2 + 0
        case ("A", "Y") => 1 + 3
        case ("B", "Y") => 2 + 3
        case ("C", "Y") => 3 + 3
        case ("A", "Z") => 2 + 6
        case ("B", "Z") => 3 + 6
        case ("C", "Z") => 1 + 6
      }
  }

  case class Round(opponent: Move, player: Move)

  case class Context(solve: List[Round] => Int)

  object Runner extends Day[List[Round], Context, Int](2022, 2) {
    override def transformInput(lines: List[String]): List[Round] =
      lines.map { line =>
        val split = line.split(' ')
        Round(Move(split(0)), Move(split(1)))
      }.toList

    override def partOneContext(): Option[Context] =
      Some(Context(_.map(r => r.player.score(r.opponent)).sum))

    override def partTwoContext(): Option[Context] = {
      Some(Context(_.map(r => r.player.score2(r.opponent)).sum))
    }

    override def process(input: List[Round], context: Option[Context]): Option[Int] = {
      context.map(_.solve(input))
    }
  }

  override def run(args: List[String]): IO[ExitCode] = Runner.run(args)
}
