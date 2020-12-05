package adventofcode.year2015

import adventofcode.Day

case class Day4Context(leadingZeros: Int)

object Day4 extends Day[String, Day4Context](2015, 4) {

  override def transformInput(lines: List[String]): String =
    lines.mkString

  override def partOneContext(): Option[Day4Context] =
    Some(Day4Context(5))

  override def partTwoContext(): Option[Day4Context] =
    Some(Day4Context(6))

  override def process(input: String, context: Option[Day4Context]): Option[Long] = {
    def hash(str: String): Array[Byte] = java.security.MessageDigest.getInstance("MD5").digest(str.getBytes)

    context.flatMap { ctx =>
      def passes(hash: Array[Byte]): Boolean = hash(0) == 0 && hash(1) == 0 && hash
        .map("%02x".format(_))
        .mkString
        .takeWhile(_ == '0')
        .length >= ctx.leadingZeros

      LazyList.from(1).map(n => (n, hash(input + n))).find(n => passes(n._2)).map(_._1.toLong)
    }
  }
}
