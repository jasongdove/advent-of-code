package aoc2020

import cats.effect._

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

case class Seat(row: Int, column: Int) {
  val id: Int = row * 8 + column
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

object Day5 extends IOApp {
  val rowTree = BSPTree.from(Range(0, 128).toList)
  val colTree = BSPTree.from(Range(0, 8).toList)
  val plane = Plane(rowTree, colTree)

  override def run(args: List[String]): IO[ExitCode] = {
    for {
      input <- readInput("day5.txt")
      resultOne <- IO(maxId(input))
      _ <- printResult(Some(resultOne))
      resultTwo <- IO(mySeat(input))
      _ <- printResult(resultTwo)
    } yield ExitCode.Success
  }

  def readInput(resourceName: String): IO[List[SeatSpecifier]] = IO {
    os.read(os.resource / resourceName)
      .split("\n")
      .toList
      .map(SeatSpecifier.from)
  }

  def maxId(specs: List[SeatSpecifier]): Int =
    specs.flatMap(plane.seat(_)).map(_.id).max

  def mySeat(specs: List[SeatSpecifier]): Option[Int] = {
    val allSeats = (for {
      r <- Range(0, 128)
      c <- Range(0, 8)
    } yield Seat(r, c)).toList
    val allSeatIds = allSeats.map(_.id)

    val bookedSeats = specs.flatMap(plane.seat(_))
    val missingSeats = allSeats.filter(!bookedSeats.contains(_))
    val mySeat = missingSeats.find(s =>
      s.row != 0 && s.row != 127 && allSeatIds.contains(s.id + 1) && allSeatIds.contains(s.id - 1)
    )
    mySeat.map(_.id)
  }

  def printResult(result: Option[Int]): IO[Unit] =
    IO(println(s"seat id: $result"))
}
