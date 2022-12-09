package adventofcode.year2022

import adventofcode.Day
import cats.effect._

object Day4 extends IOApp {

  case class AssignmentPair(one: Range, two: Range) {
    def fullyContains: Boolean = (two.contains(one.head) && two.contains(one.last)) || (one.contains(two.head) && one.contains(two.last))
    def intersects: Boolean = !two.intersect(one).isEmpty
  }

  case class Context(solve: List[AssignmentPair] => Int)

  object Runner extends Day[List[AssignmentPair], Context, Int](2022, 4) {
    override def transformInput(lines: List[String]): List[AssignmentPair] =
      lines.map { line =>
        var split = line.split(',')
        var oneSplit = split(0).split('-')
        var twoSplit = split(1).split('-')
        var one = oneSplit.head.toInt.to(oneSplit.last.toInt)
        var two = twoSplit.head.toInt.to(twoSplit.last.toInt)
        AssignmentPair(one, two)
      }.toList

    override def partOneContext(): Option[Context] =
      Some(Context(_.count(_.fullyContains)))

    override def partTwoContext(): Option[Context] =
      Some(Context(_.count(_.intersects)))

    override def process(input: List[AssignmentPair], context: Option[Context]): Option[Int] =
      context.map(_.solve(input))
  }

  override def run(args: List[String]): IO[ExitCode] = Runner.run(args)
}
