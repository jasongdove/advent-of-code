package adventofcode.year2022

import adventofcode.Day
import cats.effect._

object Day24 extends IOApp {
  case class ValleyLocation(row: Int, col: Int) {
    def hashKey(cols: Int): Int = row * cols + col
  }

  case class Valley(
    rows: Int,
    cols: Int,
    entrance: ValleyLocation,
    exit: ValleyLocation,
    upBlizzards: List[ValleyLocation],
    rightBlizzards: List[ValleyLocation],
    downBlizzards: List[ValleyLocation],
    leftBlizzards: List[ValleyLocation]
  ) {
    def printValley(expedition: ValleyLocation, up: Set[Int], right: Set[Int], down: Set[Int], left: Set[Int]): Unit = {
      for (r <- 0 until rows) {
        for (c <- 0 until cols) {
          val location = ValleyLocation(r, c)
          if (location == expedition) {
            print('E')
          } else if (location == entrance || location == exit) {
            print('.')
          } else if (r == 0 || r == rows - 1 || c == 0 || c == cols - 1) {
            print('#')
          } else {
            var char: Char = '.'
            var count = 0

            if (up.contains(location.hashKey(cols))) {
              count = count + 1
              char = '^'
            }

            if (right.contains(location.hashKey(cols))) {
              count = count + 1
              char = '>'
            }

            if (down.contains(location.hashKey(cols))) {
              count = count + 1
              char = 'v'
            }

            if (left.contains(location.hashKey(cols))) {
              count = count + 1
              char = '<'
            }

            if (count <= 1) {
              print(char)
            } else {
              print(count)
            }
          }
        }
        println()
      }
    }
  }

  case class Input(valley: Valley)

  case object Input {
    def from(lines: List[String]): Input = {
      val rows = lines.size
      val cols = lines.head.size
      val upBlizzards = scala.collection.mutable.ArrayBuffer.empty[ValleyLocation]
      val rightBlizzards = scala.collection.mutable.ArrayBuffer.empty[ValleyLocation]
      val downBlizzards = scala.collection.mutable.ArrayBuffer.empty[ValleyLocation]
      val leftBlizzards = scala.collection.mutable.ArrayBuffer.empty[ValleyLocation]

      lines.zipWithIndex.foreach { case (line, row) =>
        line.zipWithIndex.foreach {
          case ('^', col) =>
            upBlizzards.addOne(ValleyLocation(row, col))
          case ('>', col) =>
            rightBlizzards.addOne(ValleyLocation(row, col))
          case ('v', col) =>
            downBlizzards.addOne(ValleyLocation(row, col))
          case ('<', col) =>
            leftBlizzards.addOne(ValleyLocation(row, col))
          case _ => ()
        }
      }

      Input(
        Valley(
          rows,
          cols,
          ValleyLocation(0, lines.head.indexOf('.')),
          ValleyLocation(rows - 1, lines.last.indexOf('.')),
          upBlizzards.toList,
          rightBlizzards.toList,
          downBlizzards.toList,
          leftBlizzards.toList
        )
      )
    }
  }

  def moveLocation(
    location: ValleyLocation,
    delta: ValleyLocation,
    rows: Int,
    cols: Int,
    exit: ValleyLocation,
    canExit: Boolean
  ): ValleyLocation =
    (location.row + delta.row, location.col + delta.col) match {
      case (exit.row, exit.col) if canExit    => exit
      case (r, c) if !canExit && r < 1        => ValleyLocation(rows - 2 - math.abs(r) % (rows - 2), c)
      case (r, c) if !canExit && r > rows - 2 => ValleyLocation((r - 1) % (rows - 2) + 1, c)
      case (r, c) if !canExit && c < 1        => ValleyLocation(r, cols - 2 - math.abs(c) % (cols - 2))
      case (r, c) if !canExit && c > cols - 2 => ValleyLocation(r, (c - 1) % (cols - 2) + 1)
      case (r, c)                             => ValleyLocation(r, c)
    }

  def moveBlizzards(blizzards: List[ValleyLocation], delta: ValleyLocation, rows: Int, cols: Int): Set[Int] =
    blizzards.map { loc =>
      moveLocation(loc, delta, rows, cols, ValleyLocation(0, 0), false).hashKey(cols)
    }.toSet

  def solve(valley: Valley, roundOffset: Int): Int = {
    case class ValleyState(expedition: ValleyLocation, rounds: Int)

    def minDist(location: ValleyLocation): Int =
      math.abs(location.row - valley.exit.row) + math.abs(location.col - valley.exit.col)

    def priorityOrder(s: ValleyState): Int = -minDist(s.expedition) * s.rounds
    val visited = scala.collection.mutable.Set.empty[ValleyState]
    val queue = scala.collection.mutable.PriorityQueue.empty[ValleyState](Ordering.by(priorityOrder))
    var best: Int = Int.MaxValue

    queue.addOne(ValleyState(valley.entrance, roundOffset))

    while (!queue.isEmpty) {
      val state = queue.dequeue()
      val ValleyState(expedition, roundNumber) = state
      if (expedition == valley.exit) {
        if (roundNumber < best) {
          best = roundNumber
        }
      } else if (!visited.contains(state) && roundNumber + minDist(expedition) < best) {
        visited.addOne(state)
        val nextRound = roundNumber + 1

        val up = moveBlizzards(valley.upBlizzards, ValleyLocation(-nextRound, 0), valley.rows, valley.cols)
        val right = moveBlizzards(valley.rightBlizzards, ValleyLocation(0, nextRound), valley.rows, valley.cols)
        val down = moveBlizzards(valley.downBlizzards, ValleyLocation(nextRound, 0), valley.rows, valley.cols)
        val left = moveBlizzards(valley.leftBlizzards, ValleyLocation(0, -nextRound), valley.rows, valley.cols)

        val nextStates = List(
          expedition,
          moveLocation(expedition, ValleyLocation(-1, 0), valley.rows, valley.cols, valley.exit, true),
          moveLocation(expedition, ValleyLocation(0, 1), valley.rows, valley.cols, valley.exit, true),
          moveLocation(expedition, ValleyLocation(1, 0), valley.rows, valley.cols, valley.exit, true),
          moveLocation(expedition, ValleyLocation(0, -1), valley.rows, valley.cols, valley.exit, true)
        )
          .filter { loc =>
            loc == valley.entrance || loc == valley.exit || (loc.row >= 1 && loc.col >= 1 && loc.row <= valley.rows - 2 && loc.col <= valley.cols - 2)
          }
          .filter { loc =>
            val hashKey = loc.hashKey(valley.cols)
            !up.contains(hashKey) && !right.contains(hashKey) && !down.contains(hashKey) && !left.contains(hashKey)
          }
          .map(ValleyState(_, nextRound))

        queue.addAll(nextStates)
      }
    }

    best
  }

  case class Context(solve: Input => Int)

  object Runner extends Day[Input, Context, Int](2022, 24) {
    override def transformInput(lines: List[String]): Input =
      Input.from(lines)

    override def partOneContext(): Option[Context] =
      Some(Context { input =>
        solve(input.valley, 0)
      })

    override def partTwoContext(): Option[Context] =
      Some(Context { input =>
        val one = solve(input.valley, 0)
        val two = solve(input.valley.copy(entrance = input.valley.exit, exit = input.valley.entrance), one)
        solve(input.valley, two)
      })

    override def process(input: Input, context: Option[Context]): Option[Int] =
      context.map(_.solve(input))
  }

  override def run(args: List[String]): IO[ExitCode] = Runner.run(args)
}
