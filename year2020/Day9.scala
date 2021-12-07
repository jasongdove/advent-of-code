package adventofcode.year2020

import adventofcode.Day

case class Day9Context(process: List[Long] => Option[Long])

object Day9 extends Day[List[Long], Day9Context, Long](2020, 9) {
  override def transformInput(lines: List[String]): List[Long] =
    lines.map(_.toLong)

  override def partOneContext(): Option[Day9Context] =
    Some(Day9Context(processPartOne(25)))

  override def partTwoContext(): Option[Day9Context] =
    Some(Day9Context(processPartTwo))

  override def process(input: List[Long], context: Option[Day9Context]): Option[Long] =
    context.flatMap(_.process(input))

  def processPartOne(preambleLength: Int)(input: List[Long]): Option[Long] =
    findInvalid(input, preambleLength)

  private def processPartTwo(input: List[Long]): Option[Long] =
    partOneResult.flatMap { target =>
      findContiguous(input, target).map(result => result.min + result.max)
    }

  private def findInvalid(list: List[Long], preambleLength: Int): Option[Long] = {
    @annotation.tailrec
    def loop(preamble: List[Long], currentIndex: Int): Option[Long] = {
      val maybeTarget = list.lift(currentIndex)
      maybeTarget match {
        case None => None
        case Some(target) =>
          if (preamble.combinations(2).forall(_.sum != target)) Some(target)
          else loop(preamble.drop(1) :+ target, currentIndex + 1)
      }
    }

    loop(list.slice(0, preambleLength), preambleLength)
  }

  private def findContiguous(list: List[Long], target: Long): Option[List[Long]] = {
    (2 to list.indexOf(target)).flatMap { length =>
      list.sliding(length).find(_.sum == target)
    }.headOption
  }
}
