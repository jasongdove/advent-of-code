package adventofcode.year2015

import adventofcode.Day
import cats.effect._

object Day17 extends IOApp {
  case class Container(id: Int, liters: Int)
  case class Context(liters: Int, aggregate: List[List[Container]] => Int)

  def minBucketOptions(allOptions: List[List[Container]]): Int = {
    val minOption = allOptions.sortWith(_.length < _.length).head.length
    allOptions.count(_.length == minOption)
  }

  object Runner extends Day[List[Container], Context, Int](2015, 17) {

    override def transformInput(lines: List[String]): List[Container] =
      lines.zipWithIndex.map { case (l, i) => Container(i, l.toInt) }

    override def partOneContext(): Option[Context] =
      Some(Context(150, _.length))

    override def partTwoContext(): Option[Context] =
      Some(Context(150, minBucketOptions))

    override def process(input: List[Container], context: Option[Context]): Option[Int] = context.map { ctx =>
      ctx.aggregate(
        Range(1, input.length + 1)
          .flatMap(i => input.combinations(i))
          .toList
          .filter(_.map(_.liters).sum == ctx.liters)
      )
    }
  }

  override def run(args: List[String]): IO[ExitCode] = Runner.run(args)
}
