package adventofcode.year2016

import adventofcode.Day

case class Day9Context(process: String => Long)

object Day9 extends Day[String, Day9Context, Long](2016, 9) {

  override def transformInput(lines: List[String]): String =
    lines.mkString

  override def partOneContext(): Option[Day9Context] =
    Some(Day9Context(decompressedLength))

  override def partTwoContext(): Option[Day9Context] =
    Some(Day9Context(fullyDecompressedLength))

  override def process(input: String, context: Option[Day9Context]): Option[Long] = context.map(_.process(input))

  private def decompressedLength(input: String): Long = {
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

  private def fullyDecompressedLength(input: String): Long = {
    val pattern = "([A-Z]*)\\((\\d+)x(\\d+)\\)".r
    @annotation.tailrec
    def loop(acc: Long, remaining: String): Long = {
      pattern.findFirstMatchIn(remaining) match {
        case None => acc + remaining.length
        case Some(m) =>
          val skip = m.group(1).length
          val length = m.group(2).toInt
          val times = m.group(3).toInt
          val repeated = fullyDecompressedLength(remaining.slice(m.end, m.end + length)) * times
          loop(acc + skip + repeated, remaining.substring(m.end + length))
      }
    }
    loop(0, input)
  }
}
