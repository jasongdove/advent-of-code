package adventofcode.year2016

import adventofcode.Day
import cats.effect._

object Day5 extends IOApp {
  case class Context(process: String => String)

  private def processPartOne(input: String): String = {
    @annotation.tailrec
    def loop(password: String, index: Int): String = {
      if (password.length >= 8) password
      else {
        val next = fiveZeros(input, index)
        loop(password + next._2.charAt(5), next._1 + 1)
      }
    }

    loop("", 0)
  }

  private def processPartTwo(input: String): String = {
    @annotation.tailrec
    def loop(password: Map[Int, Char], index: Int): String = {
      if ((0 to 7).forall(password.keySet.contains))
        password.toList.sortBy(_._1).take(8).map(_._2).mkString
      else {
        val next = fiveZeros(input, index)
        val maybePosition = next._2.charAt(5).toString.toIntOption
        maybePosition match {
          case None => loop(password, next._1 + 1)
          case Some(position) =>
            if (password.contains(position))
              loop(password, next._1 + 1)
            else
              loop(password.updated(position, next._2.charAt(6)), next._1 + 1)
        }
      }
    }

    loop(Map.empty, 0)
  }

  private def fiveZeros(input: String, start: Int): (Int, String) = {
    def hash(str: String): Array[Byte] = java.security.MessageDigest.getInstance("MD5").digest(str.getBytes)

    def passes(hash: Array[Byte]): Boolean = hash(0) == 0 && hash(1) == 0 && hash
      .map("%02x".format(_))
      .mkString
      .takeWhile(_ == '0')
      .length >= 5

    LazyList
      .from(start)
      .map(n => (n, hash(input + n)))
      .find(n => passes(n._2))
      .map { case (finish, hash) => (finish, hash.map("%02x".format(_)).mkString) }
      .head
  }

  object Runner extends Day[String, Context, String](2016, 5) {
    override def transformInput(lines: List[String]): String =
      lines.mkString.trim

    override def partOneContext(): Option[Context] =
      Some(Context(processPartOne))

    override def partTwoContext(): Option[Context] =
      Some(Context(processPartTwo))

    override def process(input: String, context: Option[Context]): Option[String] =
      context.map(_.process(input))
  }

  override def run(args: List[String]): IO[ExitCode] = Runner.run(args)
}
