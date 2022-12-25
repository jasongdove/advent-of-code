package adventofcode.year2022

import adventofcode.Day
import cats.effect._

object Day25 extends IOApp {
  case class Input(numbers: List[String])

  case object Input {
    def from(lines: List[String]): Input = {
      Input(lines)
    }
  }

  def fromSnafu(number: String): Long = {
    @annotation.tailrec
    def loop(acc: Long, remaining: List[Char], place: Int): Long = {
      remaining match {
        case Nil => acc
        case head :: tail =>
          head match {
            case '-' =>
              loop(acc - Math.pow(5, place.toDouble).toLong, tail, place - 1)
            case '=' =>
              loop(acc - (2 * Math.pow(5, place.toDouble).toLong), tail, place - 1)
            case _ =>
              loop(acc + (head.toString.toLong * Math.pow(5, place.toDouble).toLong), tail, place - 1)
          }
      }
    }
    loop(0, number.toList, number.size - 1)
  }

  def toSnafu(number: Long): String = {
    @annotation.tailrec
    def loop(acc: List[Char], remaining: Long): String = {
      if (remaining == 0) acc.mkString
      else {
        val res = (remaining + 2) / 5
        val rem = remaining % 5
        val letter = rem match {
          case 0 => '0'
          case 1 => '1'
          case 2 => '2'
          case 3 => '='
          case 4 => '-'
        }
        loop(letter +: acc, res)
      }
    }
    loop(List.empty, number)
  }

  case class Context(solve: Input => String)

  object Runner extends Day[Input, Context, String](2022, 25) {
    override def transformInput(lines: List[String]): Input =
      Input.from(lines)

    override def partOneContext(): Option[Context] =
      Some(Context { input =>
        toSnafu(input.numbers.map(fromSnafu).sum)
      })

    override def partTwoContext(): Option[Context] =
      Some(Context { input => "" })

    override def process(input: Input, context: Option[Context]): Option[String] =
      context.map(_.solve(input))
  }

  override def run(args: List[String]): IO[ExitCode] = Runner.run(args)
}
