package adventofcode.year2020

import adventofcode.Day

case class Day13Notes(earliestTimestamp: Int, busses: List[Option[Int]])
case class Day13Context(process: Day13Notes => Option[Long])

object Day13 extends Day[Day13Notes, Day13Context, Long](2020, 13) {
  override def transformInput(lines: List[String]): Day13Notes =
    Day13Notes(lines(0).toInt, lines(1).split(",").map(_.toIntOption).toList)

  override def partOneContext(): Option[Day13Context] =
    Some(Day13Context(processPartOne))

  override def partTwoContext(): Option[Day13Context] =
    Some(Day13Context(processPartTwo))

  override def process(input: Day13Notes, context: Option[Day13Context]): Option[Long] =
    context.flatMap(_.process(input))

  private def processPartOne(input: Day13Notes): Option[Long] = {
    LazyList
      .from(input.earliestTimestamp)
      .flatMap(t => input.busses.flatten.map(t -> _))
      .find { case (t, b) => t % b == 0 }
      .map { case (t, b) => (t - input.earliestTimestamp) * b.toLong }
  }

  private def processPartTwo(input: Day13Notes): Option[Long] = {
    // TODO: there's got to be a more efficient way to solve this system
    val flattened = input.busses.zipWithIndex.filter(_._1.isDefined).map { case (b, i) => b.get -> i }

    @annotation.tailrec
    def loop(current: Long, increment: Int): Long = {
      if (flattened.forall { case (b, i) => (current + i) % b == 0 }) current
      else loop(current + increment, increment)
    }
    val max = input.busses.flatten.max
    val start = (100000000000000L / max) * max + max - input.busses.indexOf(Some(max))
    Some(loop(start, max))
  }
}
