package adventofcode.year2021

import cats.effect._
import adventofcode._

object Day10 extends IOApp {

  case class Input(lines: List[String])

  case object Input {
    def from(input: List[String]): Input = {
      Input(input)
    }
  }

  case class Context(process: Input => Long)

  object Runner extends Day[Input, Context, Long](2021, 10) {
    override def transformInput(lines: List[String]): Input =
      Input.from(lines)

    override def partOneContext(): Option[Context] =
      Some(Context(processPartOne))

    override def partTwoContext(): Option[Context] =
      Some(Context(processPartTwo))

    override def process(input: Input, context: Option[Context]): Option[Long] =
      context.map(_.process(input))

    private def processPartOne(input: Input): Long =
      input.lines.map(score).sum

    private def processPartTwo(input: Input): Long = {
      val closings = input.lines
        .filter(l => score(l) == 0)
        .map(closing)
        .map(scoreClosing)

      closings.sorted.toList((closings.length / 2).toInt)
    }

    private def score(line: String): Int = {
      @annotation.tailrec
      def loop(open: Vector[Char], pos: Int): Int = {
        if (pos == line.length) 0
        else if (isOpen(line(pos))) loop(line(pos) +: open, pos + 1)
        else {
          val matches = pair(open.head) == line(pos)
          if (matches) loop(open.tail, pos + 1)
          else
            line(pos) match {
              case ')' => 3
              case ']' => 57
              case '}' => 1197
              case '>' => 25137
            }
        }
      }

      loop(Vector.empty, 0)
    }

    private def closing(line: String): String = {
      @annotation.tailrec
      def loop(open: Vector[Char], pos: Int): String = {
        if (pos == line.length) open.map(pair).mkString
        else if (isOpen(line(pos))) loop(line(pos) +: open, pos + 1)
        else {
          val matches = pair(open.head) == line(pos)
          if (matches) loop(open.tail, pos + 1)
          else ""
        }
      }

      loop(Vector.empty, 0)
    }

    private def scoreClosing(s: String): Long =
      s.foldLeft(0L)((score, c) => score * 5L + closingpoint(c))

    private def closingpoint(c: Char): Long = c match {
      case ')' => 1
      case ']' => 2
      case '}' => 3
      case '>' => 4
      case _   => 0
    }

    private def pair(c: Char): Char = c match {
      case '(' => ')'
      case '[' => ']'
      case '{' => '}'
      case '<' => '>'
      case _   => ' '
    }

    private def isOpen(c: Char) = List('(', '{', '[', '<').contains(c)
  }

  override def run(args: List[String]): IO[ExitCode] = Runner.run(args)
}
