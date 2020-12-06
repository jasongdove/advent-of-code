package adventofcode.year2015

import adventofcode.Day

case class Day10Context(rounds: Int)

object Day10 extends Day[String, Day10Context, Long](2015, 10) {

  override def transformInput(lines: List[String]): String =
    lines.mkString

  override def partOneContext(): Option[Day10Context] =
    Some(Day10Context(40))

  override def partTwoContext(): Option[Day10Context] =
    Some(Day10Context(50))

  override def process(input: String, context: Option[Day10Context]): Option[Long] =
    context.map { ctx =>
      val result = Range(0, ctx.rounds).foldLeft(input)((s, _) => lookAndSay(s))
      result.length.toLong
    }

  private def lookAndSay(input: String): String = {
    val sb = new StringBuilder

    @annotation.tailrec
    def loop(current: Char, list: List[Char], count: Int): String = {
      list match {
        case Nil => {
          sb.append(count)
          sb.append(current)
          sb.toString
        }
        case head :: tail if head == current => loop(current, tail, count + 1)
        case head :: tail => {
          sb.append(count)
          sb.append(current)
          loop(head, tail, 1)
        }
      }
    }
    loop(input.charAt(0), input.toList, 0)
  }
}
