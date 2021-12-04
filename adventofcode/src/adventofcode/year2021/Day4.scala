package adventofcode.year2021

import adventofcode.Day
import cats.effect._

object Day4 extends IOApp {

  case class Board(arr: List[Int]) {
    def isWinner(numbers: List[Int]): Boolean = {
      val wins = List(
        // rows
        0 to 4,
        5 to 9,
        10 to 14,
        15 to 19,
        20 to 24,
        // cols
        0 to 20 by 5,
        1 to 21 by 5,
        2 to 22 by 5,
        3 to 23 by 5,
        4 to 24 by 5
      )

      wins.exists(r => r.forall(i => numbers.contains(arr(i))))
    }
  }

  case object Board {
    def from(lines: List[String]): Board =
      Board(lines.mkString(" ").split(' ').flatMap(_.toIntOption).toList)
  }

  case class Input(numbers: List[Int], boards: List[Board])

  case object Input {
    def from(input: List[String]): Input = {
      val numbers = input.head.split(',').map(_.toInt).toList
      val toChunk = input.slice(2, input.length)
      val boards = toChunk.grouped(6).map(_.slice(0, 5)).map(Board.from).toList
      Input(numbers, boards)
    }
  }

  case class Winner(numbers: List[Int], board: Board)

  case class Context(process: Input => Winner)

  object Runner extends Day[Input, Context, Int](2021, 4) {
    override def transformInput(lines: List[String]): Input =
      Input.from(lines)

    override def partOneContext(): Option[Context] =
      Some(Context(processPartOne))

    override def partTwoContext(): Option[Context] =
      Some(Context(processPartTwo))

    override def process(input: Input, context: Option[Context]): Option[Int] =
      context.map(ctx => {
        val w = ctx.process(input)
        w.numbers.last * w.board.arr.filterNot(w.numbers.contains).sum
      })

    private def processPartOne(input: Input): Winner = {
      @annotation.tailrec
      def loop(len: Int): (List[Int], Board) = {
        val slice = input.numbers.take(len).toList
        val winner = input.boards.filter(b => b.isWinner(slice))
        if (winner.length == 1)
          (slice, winner.head)
        else
          loop(len + 1)
      }

      val firstToWin = loop(1)
      Winner(firstToWin._1, firstToWin._2)
    }

    private def processPartTwo(input: Input): Winner = {
      @annotation.tailrec
      def loop(len: Int, acc: List[Board]): (List[Int], Board) = {
        val slice = input.numbers.take(len).toList
        val nonWinners = acc.filterNot(b => b.isWinner(slice))
        if (acc.length == 1 && nonWinners.length == 0)
          (slice, acc.head)
        else
          loop(len + 1, nonWinners)
      }

      val lastToWin = loop(1, input.boards)
      Winner(lastToWin._1, lastToWin._2)
    }
  }

  override def run(args: List[String]): IO[ExitCode] = Runner.run(args)
}
