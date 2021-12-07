package adventofcode.year2015

import adventofcode.Day
import cats.effect._

object Day24 extends IOApp {
  case class Context(groups: Int)

  private def fewestEqualWeight(input: Set[Long], weight: Long): Int = {
    (1 to input.size).foldLeft(0) { (acc, i) =>
      if (acc > 0) acc
      else if (input.subsets(i).exists(_.sum == weight)) i
      else 0
    }
  }

  object Runner extends Day[Set[Long], Context, Long](2015, 24) {

    override def transformInput(lines: List[String]): Set[Long] =
      lines.map(_.toLong).toSet

    override def partOneContext(): Option[Context] =
      Some(Context(3))

    override def partTwoContext(): Option[Context] =
      Some(Context(4))

    override def process(input: Set[Long], context: Option[Context]): Option[Long] =
      context.map { ctx =>
        val targetWeight = input.sum / ctx.groups
        val fewest = fewestEqualWeight(input, targetWeight)
        input.subsets(fewest).filter(_.sum == targetWeight).map(_.product).min
      }
  }

  override def run(args: List[String]): IO[ExitCode] = Runner.run(args)
}
