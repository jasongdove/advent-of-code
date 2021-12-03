package adventofcode.year2021

import adventofcode.Day
import cats.effect._
import adventofcode.utils._

object Day3 extends IOApp {

  case class Context(process: List[String] => Int)

  object Runner extends Day[List[String], Context, Int](2021, 3) {
    override def transformInput(lines: List[String]): List[String] =
      lines

    override def partOneContext(): Option[Context] =
      Some(Context(input => commonString(input, '1') * commonString(input, '0')))

    override def partTwoContext(): Option[Context] =
      Some(Context(input => longestCommonSubstring(input, '1', '0') * longestCommonSubstring(input, '0', '1')))

    override def process(input: List[String], context: Option[Context]): Option[Int] =
      context.map(_.process(input))

    private def commonString(input: List[String], target: Char): Int =
      (0 until input.head.length)
        .map(i => {
          val a = input.count(l => l(i) == target)
          val b = input.count(l => l(i) != target)
          if (a > b) '1' else '0'
        })
        .mkString
        .parseBinaryToInt()

    private def longestCommonSubstring(input: List[String], positive: Char, negative: Char): Int = {
      @annotation.tailrec
      def loop(i: Int, acc: List[String]): String = {
        if (acc.size == 1) acc.head
        else {
          val one = acc.count(s => s(i) == '1')
          val zero = acc.count(s => s(i) == '0')
          val filter = if (one >= zero) positive else negative
          loop(i + 1, acc.filter(s => s(i) == filter))
        }
      }

      loop(0, input).parseBinaryToInt()
    }
  }

  override def run(args: List[String]): IO[ExitCode] = Runner.run(args)
}
