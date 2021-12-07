package adventofcode.year2015

import adventofcode.Day
import cats.effect._

object Day11 extends IOApp {
  // format: off
  private val pairs = List("aa", "bb", "cc", "dd", "ee", "ff", "gg", "hh", "ii", "jj", "kk", "ll", "mm", "nn", "oo", "pp", "qq", "rr", "ss", "tt", "uu", "vv", "ww", "xx", "yy", "zz")
  private val alphabet = "abcdefghijklmnopqrstuvwxyz"
  private val triples = Range(0, 24).map(i => alphabet.substring(i, i + 3)).toList
  // format: on

  case class Context(iterations: Int)

  private def fromBase26(num: String): Long = {
    @annotation.tailrec
    def loop(acc: Long, remaining: List[Char], place: Int): Long = {
      remaining match {
        case Nil => acc
        case head :: tail =>
          loop(acc + (head - 'a').toInt.toLong * Math.pow(26, place.toDouble).toLong, tail, place - 1)
      }
    }
    loop(0, num.toList, num.length - 1)
  }

  private def toBase26(num: Long): String = {
    @annotation.tailrec
    def loop(acc: List[Char], remaining: Long): String = {
      if (remaining == 0) acc.mkString
      else {
        val res = remaining / 26
        val rem = remaining % 26
        val letter = ('a' + rem).toChar
        loop(letter +: acc, res)
      }
    }
    loop(List.empty, num)
  }

  private def increasingThree(password: String): Boolean =
    triples.exists(password.contains)

  private def noBadLetters(password: String): Boolean =
    !password.contains("i") && !password.contains("o") && !password.contains("l")

  private def diffNonOverlap(password: String): Boolean =
    pairs.count(password.contains) >= 2

  object Runner extends Day[String, Context, String](2015, 11) {
    override def transformInput(lines: List[String]): String =
      lines.mkString

    override def partOneContext(): Option[Context] =
      Some(Context(1))

    override def partTwoContext(): Option[Context] =
      Some(Context(2))

    override def process(input: String, context: Option[Context]): Option[String] =
      context.map { ctx =>
        val current = fromBase26(input)
        LazyList
          .from(1)
          .map(i => toBase26(current + i))
          .filter(p => increasingThree(p) && noBadLetters(p) && diffNonOverlap(p))
          .take(ctx.iterations)
          .toList(ctx.iterations - 1)
      }
  }

  override def run(args: List[String]): IO[ExitCode] = Runner.run(args)
}
