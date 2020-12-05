package adventofcode.year2015

import adventofcode.Day

case class Day8Context(process: String => String)

object Day8 extends Day[List[String], Day8Context](2015, 8) {
  override def transformInput(lines: List[String]): List[String] = lines

  override def partOneContext(): Option[Day8Context] =
    Some(Day8Context(unescape))

  override def partTwoContext(): Option[Day8Context] =
    Some(Day8Context(escape))

  override def process(input: List[String], context: Option[Day8Context]): Option[Long] =
    context.map { ctx =>
      input.map(line => (line.length - ctx.process(line).length).abs).sum.toLong
    }

  private def unescape(s: String): String = {
    val hex =
      List('a', 'b', 'c', 'd', 'e', 'f', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F')
    val input = s.replaceAll("^\"|\"$", "")
    def loop(isEscaped: Boolean, buffer: Seq[Char], remaining: Seq[Char]): Seq[Char] = {
      if (isEscaped) {
        remaining match {
          case Seq('"', tail @ _*)  => loop(false, buffer ++ Seq('"'), tail)
          case Seq('\\', tail @ _*) => loop(false, buffer ++ Seq('\\'), tail)
          case Seq('x', one, two, tail @ _*) =>
            if (hex.contains(one) && hex.contains(two))
              loop(false, buffer ++ Seq('_'), tail)
            else
              loop(false, buffer ++ Seq('x'), Seq(one, two) ++ tail)
        }
      } else {
        remaining match {
          case Seq('\\', tail @ _*) => loop(true, buffer, tail)
          case Seq(head, tail @ _*) => loop(false, buffer ++ Seq(head), tail)
          case _                    => buffer
        }
      }
    }
    loop(false, Seq.empty, input).mkString
  }

  private def escape(s: String): String = {
    def loop(buffer: Seq[Char], remaining: Seq[Char]): Seq[Char] = {
      remaining match {
        case Seq('\"', tail @ _*) => loop(buffer ++ Seq('\\', '\"'), tail)
        case Seq('\\', tail @ _*) => loop(buffer ++ Seq('\\', '\\'), tail)
        case Seq(head, tail @ _*) => loop(buffer ++ Seq(head), tail)
        case _                    => buffer
      }
    }
    (loop(Seq('\"'), s) ++ Seq('\"')).mkString
  }
}
