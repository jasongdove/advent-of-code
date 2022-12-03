package adventofcode.year2022

import adventofcode.Day
import cats.effect._

object Day1 extends IOApp {

  case class Rucksack(one: List[Char], two: List[Char]) {
    def combined: List[Char] = one ++ two

    def priority(c: Char) = c match {
      case _ if 'a' to 'z' contains c => (c - 'a') + 1
      case _ if 'A' to 'Z' contains c => (c - 'A') + 27
      case _                          => 0
    }
  }

  case class Context(solve: List[Rucksack] => Int)

  object Runner extends Day[List[Rucksack], Context, Int](2022, 3) {
    override def transformInput(lines: List[String]): List[Rucksack] =
      lines.map { line =>
        var one = line.toArray.take(line.length() / 2).toList
        var two = line.toArray.takeRight(line.length() / 2).toList
        Rucksack(one, two)
      }.toList

    override def partOneContext(): Option[Context] =
      Some(Context(_.map(s => s.priority(s.one.find(s.two.contains).get)).sum))

    override def partTwoContext(): Option[Context] =
      Some(Context(_.sliding(3, 3).map(g => g.head.priority(g.head.combined.find(c => g(1).combined.contains(c) && g(2).combined.contains(c)).get)).sum))

    override def process(input: List[Rucksack], context: Option[Context]): Option[Int] = {
      context.map(_.solve(input))
    }
  }

  override def run(args: List[String]): IO[ExitCode] = Runner.run(args)
}
