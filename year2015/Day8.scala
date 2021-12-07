package adventofcode.year2015

import adventofcode.Day
import cats.effect._

import scala.annotation.tailrec

object Day8 extends IOApp {
  case class Context(process: String => String)

  private def unescape(s: String): String = {
    val hex =
      List('a', 'b', 'c', 'd', 'e', 'f', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F')
    val input = s.replaceAll("^\"|\"$", "")
    @tailrec
    def loop(isEscaped: Boolean, buffer: Seq[Char], remaining: Seq[Char]): Seq[Char] = {
      if (isEscaped) {
        remaining match {
          case Seq('"', tail @ _*)  => loop(isEscaped = false, buffer :+ '"', tail)
          case Seq('\\', tail @ _*) => loop(isEscaped = false, buffer :+ '\\', tail)
          case Seq('x', one, two, tail @ _*) =>
            if (hex.contains(one) && hex.contains(two))
              loop(isEscaped = false, buffer :+ '_', tail)
            else
              loop(isEscaped = false, buffer :+ 'x', Seq(one, two) ++ tail)
        }
      } else {
        remaining match {
          case Seq('\\', tail @ _*) => loop(isEscaped = true, buffer, tail)
          case Seq(head, tail @ _*) => loop(isEscaped = false, buffer :+ head, tail)
          case _                    => buffer
        }
      }
    }
    loop(isEscaped = false, Seq.empty, input).mkString
  }

  private def escape(s: String): String = {
    @tailrec
    def loop(buffer: Seq[Char], remaining: Seq[Char]): Seq[Char] = {
      remaining match {
        case Seq('\"', tail @ _*) => loop(buffer ++ Seq('\\', '\"'), tail)
        case Seq('\\', tail @ _*) => loop(buffer ++ Seq('\\', '\\'), tail)
        case Seq(head, tail @ _*) => loop(buffer :+ head, tail)
        case _                    => buffer
      }
    }
    (loop(Seq('\"'), s) ++ Seq('\"')).mkString
  }

  object Runner extends Day[List[String], Context, Int](2015, 8) {
    override def transformInput(lines: List[String]): List[String] = lines

    override def partOneContext(): Option[Context] =
      Some(Context(unescape))

    override def partTwoContext(): Option[Context] =
      Some(Context(escape))

    override def process(input: List[String], context: Option[Context]): Option[Int] =
      context.map { ctx =>
        input.map(line => (line.length - ctx.process(line).length).abs).sum
      }
  }

  override def run(args: List[String]): IO[ExitCode] = Runner.run(args)
}
