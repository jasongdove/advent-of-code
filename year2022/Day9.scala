//> using javaOpt "-Xms64m"
//> using javaOpt "-Xmx1g"

package adventofcode.year2022

import adventofcode.{Day, Grid, GridLocation}
import cats.effect._
import scala.collection.mutable.ArrayBuffer

object Day9 extends IOApp {

  case class Move(direction: String)

  case object Move {
    def from(line: String): Seq[Move] = {
      val split = line.split(' ')
      1.to(split(1).toInt).map(_ => Move(split(0)))
    }
  }

  case class LongLoc(row: Long, col: Long) {
    def moveHead(m: Move): LongLoc = m match {
      case Move("U") => LongLoc(row - 1, col)
      case Move("D") => LongLoc(row + 1, col)
      case Move("L") => LongLoc(row, col - 1)
      case Move(_)   => LongLoc(row, col + 1)
    }

    def moveTail(head: LongLoc): LongLoc = {
      val doNotMove = List(
        LongLoc(row - 1, col - 1),
        LongLoc(row - 1, col),
        LongLoc(row - 1, col + 1),
        LongLoc(row, col - 1),
        LongLoc(row, col),
        LongLoc(row, col + 1),
        LongLoc(row + 1, col + 1),
        LongLoc(row + 1, col),
        LongLoc(row + 1, col - 1),
      )
      if (doNotMove.contains(head)) this
      else {
        if (head.row == row) {
          if (head.col > col) LongLoc(row, col + 1)
          else LongLoc(row, col - 1)
        } else if (head.col == col) {
          if (head.row > row) LongLoc(row + 1, col)
          else LongLoc(row - 1, col)
        } else {
          val rowDirection = if (head.row > row) 1 else -1
          val colDirection = if (head.col > col) 1 else -1
          LongLoc(row + rowDirection, col + colDirection)
        }
      }
    }
  }

  case class Problem(moves: List[Move]) {
    def solve(): Int = {
      def loop(q: List[Move], hl: LongLoc, tl: LongLoc, visited: Set[LongLoc]): Int = {
        q match {
          case move :: tail => {
            val nextHeadLoc = hl.moveHead(move)
            val nextTailLoc = tl.moveTail(nextHeadLoc)
            loop(tail, nextHeadLoc, nextTailLoc, visited + nextTailLoc)
          }
          case Nil => visited.size
        }
      }
      loop(moves, LongLoc(0, 0), LongLoc(0, 0), Set(LongLoc(0, 0)))
    }
  }

  case class Context(solve: Problem => Int)

  object Runner extends Day[Problem, Context, Int](2022, 9) {
    override def transformInput(lines: List[String]): Problem =
      Problem(lines.flatMap(Move.from))

    override def partOneContext(): Option[Context] =
      Some(Context(l => l.solve()))

    override def partTwoContext(): Option[Context] =
      Some(Context(l => l.solve()))

    override def process(input: Problem, context: Option[Context]): Option[Int] = {
      context.map(_.solve(input))
    }
  }

  override def run(args: List[String]): IO[ExitCode] = Runner.run(args)
}
