package adventofcode.year2020

import adventofcode.Day

case class Day15Context(target: Int)

object Day15 extends Day[List[Int], Day15Context, Int](2020, 15) {

  override def transformInput(lines: List[String]): List[Int] =
    lines.mkString.split(",").map(_.toInt).toList

  override def partOneContext(): Option[Day15Context] =
    Some(Day15Context(2020))

  override def partTwoContext(): Option[Day15Context] =
    Some(Day15Context(30000000))

  override def process(input: List[Int], context: Option[Day15Context]): Option[Int] =
    context.map { ctx => speak(input, ctx.target) }

  private def speak(known: List[Int], target: Int): Int = {
    @annotation.tailrec
    def loop(acc: Map[Int, Int], currentIndex: Int, spokenAtIndex: Int): Int = {
      if (currentIndex == target) spokenAtIndex
      else {
        acc.get(spokenAtIndex) match {
          case None =>
            loop(acc.updated(spokenAtIndex, currentIndex), currentIndex + 1, 0)
          case Some(n) =>
            loop(acc.updated(spokenAtIndex, currentIndex), currentIndex + 1, currentIndex - n)
        }
      }
    }

    val init = known.zipWithIndex
      .slice(0, known.length - 1)
      .map { case (num, index) => (num, index + 1) }
      .toMap

    loop(init, known.length, known.last)
  }
}
