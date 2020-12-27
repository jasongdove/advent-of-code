package adventofcode.year2016

import adventofcode.Day
import cats.effect._

object Day6 extends IOApp {
  case class Context(selector: Map[Char, Int] => Char)

  object Runner extends Day[List[String], Context, String](2016, 6) {
    override def transformInput(lines: List[String]): List[String] = lines

    override def partOneContext(): Option[Context] =
      Some(Context(_.toList.minBy(-_._2)._1))

    override def partTwoContext(): Option[Context] =
      Some(Context(_.toList.minBy(_._2)._1))

    override def process(input: List[String], context: Option[Context]): Option[String] =
      context.map { ctx =>
        (0 until input.head.length).map { position =>
          ctx.selector(input.map(_.charAt(position)).groupMapReduce(identity)(_ => 1)(_ + _))
        }.mkString
      }
  }

  override def run(args: List[String]): IO[ExitCode] = Runner.run(args)
}
