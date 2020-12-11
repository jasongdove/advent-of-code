package adventofcode.year2020

import adventofcode.{Day, GridLocation}

sealed abstract class Day11SeatType

object Day11SeatType {
  case object Floor extends Day11SeatType
  case object Empty extends Day11SeatType
  case object Occupied extends Day11SeatType
}

case class Day11AnimatedGrid(elements: Map[GridLocation, Day11SeatType]) {
  val size = elements.keys.map(_.row).max

  lazy val occupied = elements.count(_._2 == Day11SeatType.Occupied)

  def debug() = {
    for (row <- 0 to elements.keySet.map(_.row).max) {
      for (col <- 0 to elements.keySet.map(_.col).max) {
        print(elements(GridLocation(row, col)) match {
          case Day11SeatType.Empty    => 'L'
          case Day11SeatType.Floor    => '.'
          case Day11SeatType.Occupied => '#'
        })
      }
      println()
    }
    println()
  }
}

object Day11AnimatedGrid {
  def defaultNeighborsOn(grid: Day11AnimatedGrid, location: GridLocation): Int = {
    val checks = List(
      GridLocation(location.row - 1, location.col - 1),
      GridLocation(location.row, location.col - 1),
      GridLocation(location.row + 1, location.col - 1),
      GridLocation(location.row - 1, location.col),
      GridLocation(location.row + 1, location.col),
      GridLocation(location.row - 1, location.col + 1),
      GridLocation(location.row, location.col + 1),
      GridLocation(location.row + 1, location.col + 1)
    )
    checks.count(c => grid.elements.getOrElse(c, Day11SeatType.Floor) == Day11SeatType.Occupied)
  }

  def seeNeighborsOn(grid: Day11AnimatedGrid, start: GridLocation): Int = {
    def occupied(current: GridLocation, delta: (Int, Int)): Boolean = {
      grid.elements.lift(current) match {
        case None                         => false
        case Some(Day11SeatType.Occupied) => true
        case Some(Day11SeatType.Empty)    => false
        case Some(Day11SeatType.Floor)    => occupied(GridLocation(current.row + delta._1, current.col + delta._2), delta)
      }
    }

    val deltas = List((-1, -1), (0, -1), (1, -1), (-1, 0), (1, 0), (-1, 1), (0, 1), (1, 1))
    deltas.count(d => occupied(GridLocation(start.row + d._1, start.col + d._2), d))
  }
}

case class Day11Context(minNeighbors: Int, neighborsOn: (Day11AnimatedGrid, GridLocation) => Int)

object Day11 extends Day[Day11AnimatedGrid, Day11Context, Int](2020, 11) {

  override def transformInput(lines: List[String]): Day11AnimatedGrid = {
    val gridMap = lines.zipWithIndex
      .map { case (line, row) =>
        line.zipWithIndex.map {
          case ('#', col) => (GridLocation(row, col), Day11SeatType.Occupied)
          case ('L', col) => (GridLocation(row, col), Day11SeatType.Empty)
          case (_, col)   => (GridLocation(row, col), Day11SeatType.Floor)
        }
      }
      .flatten
      .toMap
    Day11AnimatedGrid(gridMap)
  }

  override def partOneContext(): Option[Day11Context] =
    Some(Day11Context(4, Day11AnimatedGrid.defaultNeighborsOn))

  override def partTwoContext(): Option[Day11Context] =
    Some(Day11Context(5, Day11AnimatedGrid.seeNeighborsOn))

  override def process(input: Day11AnimatedGrid, context: Option[Day11Context]): Option[Int] =
    context.flatMap { ctx =>
      @annotation.tailrec
      def iteration(
        acc: Map[GridLocation, Day11SeatType],
        start: Day11AnimatedGrid,
        remaining: List[(GridLocation, Day11SeatType)]
      ): Day11AnimatedGrid =
        remaining match {
          case Nil => Day11AnimatedGrid(acc)
          case head :: tail =>
            val neighborsOn = ctx.neighborsOn(start, head._1)
            val next = head._2 match {
              case Day11SeatType.Empty if neighborsOn == 0                   => Day11SeatType.Occupied
              case Day11SeatType.Occupied if neighborsOn >= ctx.minNeighbors => Day11SeatType.Empty
              case _                                                         => head._2
            }
            iteration(acc.updated(head._1, next), start, tail)
        }

      val list = LazyList.iterate((input, true)) { case (i, _) =>
        val next = iteration(i.elements, i, i.elements.toList)
        val changed = i.elements.exists { case (location, seatType) => next.elements(location) != seatType }
        (next -> changed)
      }
      list.takeWhile(_._2).lastOption.map(_._1.occupied)
    }
}
