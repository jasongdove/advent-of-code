package adventofcode.year2016

import adventofcode.Day
import cats.effect._

object Day9 extends IOApp {
  case class Context(process: String => Long)

  private def processPartOne(input: String): Long = {
    val pattern = "([A-Z]*)\\((\\d+)x(\\d+)\\)".r
    @annotation.tailrec
    def loop(acc: Vector[Char], remaining: String): String = {
      pattern.findFirstMatchIn(remaining) match {
        case None => (acc ++ remaining).mkString
        case Some(m) =>
          val skip = m.group(1)
          val length = m.group(2).toInt
          val times = m.group(3).toInt
          val repeated = remaining.slice(m.end, m.end + length) * times
          loop(acc ++ skip ++ repeated, remaining.substring(m.end + length))
      }
    }
    loop(Vector.empty, input).length.toLong
  }

  private def processPartTwo(input: String): Long = {
    val pattern = "([A-Z]*)\\((\\d+)x(\\d+)\\)".r
    @annotation.tailrec
    def loop(acc: Long, remaining: String): Long = {
      pattern.findFirstMatchIn(remaining) match {
        case None => acc + remaining.length
        case Some(m) =>
          val skip = m.group(1).length
          val length = m.group(2).toInt
          val times = m.group(3).toInt
          val repeated = processPartTwo(remaining.slice(m.end, m.end + length)) * times
          loop(acc + skip + repeated, remaining.substring(m.end + length))
      }
    }
    loop(0, input)
  }

  object Runner extends Day[String, Context, Long](2016, 9) {
    override def transformInput(lines: List[String]): String =
      lines.mkString

    override def partOneContext(): Option[Context] =
      Some(Context(processPartOne))

    override def partTwoContext(): Option[Context] =
      Some(Context(processPartTwo))

    override def process(input: String, context: Option[Context]): Option[Long] =
      context.map(_.process(input))
  }

  override def run(args: List[String]): IO[ExitCode] = Runner.run(args)
}
