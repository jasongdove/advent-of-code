package adventofcode.year2020

import adventofcode.Day
import cats.effect._

object Day25 extends IOApp {
  case class Context(process: List[Int] => Int)

  private def processPartOne(input: List[Int]): Int = {
    val (publicKeyOne, publicKeyTwo) = (input.head, input.last)
    val loopSizeTwo = transformLoop(7).indexOf(publicKeyTwo)
    transformLoop(publicKeyOne).drop(loopSizeTwo).next().toInt
  }

  private def processPartTwo(input: List[Int]): Int =
    input.length

  private def transformLoop(subjectNumber: Int): Iterator[Long] =
    Iterator.iterate(1L)(value => (value * subjectNumber) % 20201227)

  object Runner extends Day[List[Int], Context, Int](2020, 25) {
    override def transformInput(lines: List[String]): List[Int] = {
      lines.map(_.toInt)
    }

    override def partOneContext(): Option[Context] =
      Some(Context(processPartOne))

    override def partTwoContext(): Option[Context] =
      Some(Context(processPartTwo))

    override def process(input: List[Int], context: Option[Context]): Option[Int] =
      context.map(_.process(input))
  }

  override def run(args: List[String]): IO[ExitCode] = Runner.run(args)
}
