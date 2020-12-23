package adventofcode.year2020

import adventofcode.Day
import cats.effect._

object Day23 extends IOApp {
  case class Context(process: List[Int] => String)

  private def processPartOne(input: List[Int]): String = {
    val adj = adjacency(input, input.length)
    val result = moveCups(adj, input.head, 100)
    val list = LazyList.iterate(("", 1)) { case (acc, i) =>
      val next = result(i)
      (s"$acc$next", next)
    }
    list(input.length - 1)._1
  }

  private def processPartTwo(input: List[Int]): String = {
    val adj = adjacency(input, 1_000_000)
    val result = moveCups(adj, input.head, 10_000_000)
    val after1 = result(1)
    val after2 = result(after1)
    val f = after1.toLong * after2.toLong
    f.toString
  }

  private def adjacency(input: List[Int], length: Int): Array[Int] = {
    // build an adjacency list of clockwise cups
    // for 123487596, adj(1) == 2, adj(8) == 7, adj(6) == 1 etc
    val adj = Array.fill(length + 1)(0)

    for (cup <- 1 until adj.length) {
      if (cup <= input.length) {
        val location = input.indexOf(cup)
        adj(cup) = if (location == input.length - 1) location + 2 else input(input.indexOf(cup) + 1)
      } else adj(cup) = cup + 1
    }

    if (length == input.length) {
      adj(input.last) = input.head
    } else {
      adj(adj.length - 1) = input.head
    }

    adj
  }

  private def moveCups(adj: Array[Int], startingCup: Int, count: Int): Array[Int] = {
    @annotation.tailrec
    def loop(currentCup: Int, currentCount: Int): Array[Int] = {
      if (currentCount > count) adj
      else {
        val one = adj(currentCup)
        val two = adj(one)
        val three = adj(two)
        val removedValues = List(one, two, three)

        adj(currentCup) = adj(three)

        var destination = -1
        var destCheck = currentCup - 1
        while (destination < 0) {
          if (destCheck == 0) destCheck = adj.length - 1
          if (!removedValues.contains(destCheck)) destination = destCheck
          else destCheck = destCheck - 1
        }

        val right = adj(destination)
        adj(destination) = one
        adj(three) = right

        loop(adj(currentCup), currentCount + 1)
      }
    }

    loop(startingCup, 1)
  }

  object Runner extends Day[List[Int], Context, String](2020, 23) {
    override def transformInput(lines: List[String]): List[Int] =
      lines.mkString.trim().map(c => c - '0').toList

    override def partOneContext(): Option[Context] =
      Some(Context(processPartOne))

    override def partTwoContext(): Option[Context] =
      Some(Context(processPartTwo))

    override def process(input: List[Int], context: Option[Context]): Option[String] =
      context.map(_.process(input))
  }

  override def run(args: List[String]): IO[ExitCode] = Runner.run(args)
}
