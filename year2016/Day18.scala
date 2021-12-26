package adventofcode.year2016

import adventofcode.Day
import cats.effect._

object Day18 extends IOApp {
  case class Context(rows: Int)

  object Runner extends Day[List[Int], Context, Long](2016, 18) {
    override def transformInput(lines: List[String]): List[Int] = lines.head.map {
      case '^' => 0
      case _   => 1
    }.toList

    override def partOneContext(): Option[Context] =
      Some(Context(40))

    override def partTwoContext(): Option[Context] =
      Some(Context(400_000))

    override def process(input: List[Int], context: Option[Context]): Option[Long] =
      context.map { ctx =>
        @annotation.tailrec
        def loop(row: Seq[Int], remaining: Int, count: Long): Long = {
          if (remaining == 0) count
          else {
            val working = 1 +: row :+ 1
            val nextRow = working
              .sliding(3)
              .map { three =>
                three match {
                  case 0 :: 0 :: 1 :: Nil => 0
                  case 1 :: 0 :: 0 :: Nil => 0
                  case 0 :: 1 :: 1 :: Nil => 0
                  case 1 :: 1 :: 0 :: Nil => 0
                  case _                  => 1
                }
              }
              .toSeq
            loop(nextRow, remaining - 1, count + nextRow.sum)
          }
        }

        loop(input, ctx.rows - 1, input.sum)
      }
  }

  override def run(args: List[String]): IO[ExitCode] = Runner.run(args)
}
