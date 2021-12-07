package adventofcode.year2020

import adventofcode.Day

sealed trait Direction

object Direction {
  case object Front extends Direction
  case object Back extends Direction
}

case class SeatSpecifier(rowDirections: List[Direction], colDirections: List[Direction])

object SeatSpecifier {
  def from(line: String): SeatSpecifier = {
    val rows = line.take(7)
    val cols = line.takeRight(3)
    val rowDirections = rows.map {
      case 'F' => Direction.Front
      case _   => Direction.Back
    }.toList
    val colDirections = cols.map {
      case 'L' => Direction.Front
      case _   => Direction.Back
    }.toList
    SeatSpecifier(rowDirections, colDirections)
  }
}

case class Day5Context(process: List[SeatSpecifier] => Option[Long])

object Day5 extends Day[List[SeatSpecifier], Day5Context, Long](2020, 5) {
  case class Seat(row: Int, column: Int) {
    val id: Long = row * 8L + column
  }

  class BSPTree(val seats: List[Int], val front: Option[BSPTree], val back: Option[BSPTree]) {
    def walk(directions: List[Direction]): Option[Leaf] = {
      @annotation.tailrec
      def loop(remaining: List[Direction], current: Option[BSPTree]): Option[Leaf] = {
        remaining match {
          case Nil =>
            current match {
              case Some(leaf: Leaf) => Some(leaf)
              case _                => None
            }
          case head :: next =>
            head match {
              case Direction.Front => loop(next, current.flatMap(_.front))
              case Direction.Back  => loop(next, current.flatMap(_.back))
            }
        }
      }
      loop(directions, Some(this))
    }
  }

  case class Leaf(value: Int) extends BSPTree(List(value), None, None)

  object BSPTree {
    def from(seats: List[Int]): BSPTree = {
      if (seats.length == 1) {
        Leaf(seats.head)
      } else {
        val mid = seats.min + ((seats.max - seats.min) / 2.0)
        val (backSeats, frontSeats) = seats.partition(_ >= mid)
        val front = BSPTree.from(frontSeats)
        val back = BSPTree.from(backSeats)
        new BSPTree(seats, Some(front), Some(back))
      }
    }
  }
  case class Plane(rowTree: BSPTree, colTree: BSPTree) {
    def seat(specifier: SeatSpecifier): Option[Seat] = {
      val row = rowTree.walk(specifier.rowDirections)
      val col = colTree.walk(specifier.colDirections)
      (row, col) match {
        case (Some(r), Some(c)) => Some(Seat(r.value, c.value))
        case _                  => None
      }
    }
  }

  private val rowTree = BSPTree.from(Range(0, 128).toList)
  private val colTree = BSPTree.from(Range(0, 8).toList)
  private val plane = Plane(rowTree, colTree)

  override def transformInput(lines: List[String]): List[SeatSpecifier] =
    lines.map(SeatSpecifier.from)

  override def partOneContext(): Option[Day5Context] =
    Some(Day5Context(maxId))

  override def partTwoContext(): Option[Day5Context] =
    Some(Day5Context(mySeat))

  override def process(input: List[SeatSpecifier], context: Option[Day5Context]): Option[Long] =
    context.flatMap(_.process(input))

  private def maxId(specs: List[SeatSpecifier]): Option[Long] =
    Some(specs.flatMap(plane.seat).map(_.id).max)

  private def mySeat(specs: List[SeatSpecifier]): Option[Long] = {
    val allSeats = (for {
      r <- Range(0, 128)
      c <- Range(0, 8)
    } yield Seat(r, c)).toList
    val allSeatIds = allSeats.map(_.id)

    val bookedSeats = specs.flatMap(plane.seat)
    val missingSeats = allSeats.filter(!bookedSeats.contains(_))
    val mySeat = missingSeats.find(s =>
      s.row != 0 && s.row != 127 && allSeatIds.contains(s.id + 1) && allSeatIds.contains(s.id - 1)
    )
    mySeat.map(_.id)
  }
}
