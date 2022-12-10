package adventofcode.year2022

import adventofcode.{Day, Grid, GridLocation}
import cats.effect._

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
      if (math.abs(row - head.row) <= 1 && math.abs(col - head.col) <= 1) this
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

  case object Rope {
    def ofLength(length: Int): List[LongLoc] = 0.to(length - 1).map(_ => LongLoc(0, 0)).toList
  }

  case class Problem(moves: List[Move]) {
    def solve(initialRope: List[LongLoc]): Int = {
      def moveAll(rope: List[LongLoc], move: Move): List[LongLoc] = {
        var buf = scala.collection.mutable.ArrayBuffer.empty[LongLoc]
        buf.addOne(rope.head.moveHead(move))
        for (t <- rope.tail) {
          buf.addOne(t.moveTail(buf.last))
        }
        buf.toList
      }

      @annotation.tailrec
      def loop(q: List[Move], rope: List[LongLoc], visited: Set[LongLoc]): Int = {
        q match {
          case move :: tail => {
            val nextLocs = moveAll(rope, move)
            loop(tail, nextLocs, visited + nextLocs.last)
          }
          case Nil => visited.size
        }
      }
      loop(moves, initialRope, Set(LongLoc(0, 0)))
    }
  }

  case class Context(ropeLength: Int)

  object Runner extends Day[Problem, Context, Int](2022, 9) {
    override def transformInput(lines: List[String]): Problem =
      Problem(lines.flatMap(Move.from))

    override def partOneContext(): Option[Context] =
      Some(Context(2))

    override def partTwoContext(): Option[Context] =
      Some(Context(10))

    override def process(input: Problem, context: Option[Context]): Option[Int] =
      context.map(ctx => input.solve(Rope.ofLength(ctx.ropeLength)))
  }

  override def run(args: List[String]): IO[ExitCode] = Runner.run(args)
}
