package adventofcode.year2020

import adventofcode.Day

case class Bus(id: Long, remainder: Long)
case class Day13Notes(earliestTimestamp: Long, busses: List[Bus])
case class Day13Context(process: Day13Notes => Long)

object Day13 extends Day[Day13Notes, Day13Context, Long](2020, 13) {
  case class BusMatch(bus: Bus, time: Long)

  override def transformInput(lines: List[String]): Day13Notes = {
    val start = lines(0).toLong
    val busses = lines(1)
      .split(",")
      .zipWithIndex
      .flatMap {
        case ("x", _)           => None
        case (busId, remainder) => Some(Bus(busId.toLong, remainder.toLong))
      }
      .toList
    Day13Notes(start, busses)
  }

  override def partOneContext(): Option[Day13Context] =
    Some(Day13Context(processPartOne))

  override def partTwoContext(): Option[Day13Context] =
    Some(Day13Context(processPartTwo))

  override def process(input: Day13Notes, context: Option[Day13Context]): Option[Long] =
    context.map(_.process(input))

  private def processPartOne(input: Day13Notes): Long = {
    val noRemainders = input.busses.map(b => Bus(b.id, 0))
    val busMatch = syncWithBus(input.earliestTimestamp, 1L, noRemainders)
    val waitForBus = busMatch.time - input.earliestTimestamp
    busMatch.bus.id * waitForBus
  }

  private def processPartTwo(input: Day13Notes): Long = {
    @annotation.tailrec
    def loop(current: Long, increment: Long, remaining: List[Bus]): Long = {
      remaining match {
        case Nil => current
        case r =>
          val busMatch = syncWithBus(current, increment, r)
          loop(busMatch.time, increment * busMatch.bus.id, r diff List(busMatch.bus))
      }
    }

    loop(input.busses.head.id, input.busses.head.id, input.busses.tail)
  }

  private def syncWithBus(start: Long, increment: Long, busses: List[Bus]): BusMatch = {
    @annotation.tailrec
    def loop(current: Long): BusMatch = {
      busses.find(b => current >= b.id && (current + b.remainder) % b.id == 0) match {
        case Some(bus) => BusMatch(bus, current)
        case None      => loop(current + increment)
      }
    }
    loop(start)
  }
}
