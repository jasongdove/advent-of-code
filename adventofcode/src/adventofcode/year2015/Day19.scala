package adventofcode.year2015

import adventofcode.Day

case class Replacement(from: String, to: String)
case class Machine(replacements: List[Replacement], start: String)

object Machine {
  private val replacementPattern = "(.*) => (.*)".r

  def from(lines: List[String]): Machine = {
    val replacementLines = lines.head.split("\n").toList
    val start = lines(1).trim
    val replacements = replacementLines.map { line =>
      val replacementPattern(from, to) = line
      Replacement(from, to)
    }
    Machine(replacements, start)
  }
}

case class Day19Context(process: Machine => Option[Int])

object Day19 extends Day[Machine, Day19Context, Int](2015, 19) {

  override def splitOn(): String = "\n\n"

  override def transformInput(lines: List[String]): Machine =
    Machine.from(lines)

  override def partOneContext(): Option[Day19Context] =
    Some(Day19Context(processPartOne))

  override def partTwoContext(): Option[Day19Context] =
    Some(Day19Context(processPartTwo))

  override def process(input: Machine, context: Option[Day19Context]): Option[Int] =
    context.flatMap(_.process(input))

  private def processPartOne(machine: Machine): Option[Int] =
    Some(step(Set.empty, 0, machine).size)

  private def processPartTwo(machine: Machine): Option[Int] = {
    def commonStarts(one: String, two: String): Int = {
      @annotation.tailrec
      def inner(acc: Int, one: List[Char], two: List[Char]): Int = {
        (one, two) match {
          case (Nil, _) => acc
          case (_, Nil) => acc
          case (h1 :: t1, h2 :: t2) =>
            if (h1 == h2) {
              inner(acc + 1, t1, t2)
            } else acc
        }
      }
      inner(0, one.toList, two.toList)
    }

    val list = LazyList.iterate(Set("e")) { a =>
      val all = a
        .flatMap(b => step(Set.empty, 0, Machine(machine.replacements, b)))
        .map(s => (s, commonStarts(s, machine.start)))

      val top5Common = all.toList.map(_._2).distinct.sorted.takeRight(5)

      all.filter(t => top5Common.contains(t._2)).map(_._1)
    }
    list.zipWithIndex
      .map { case (l, i) => (i, l.contains(machine.start)) }
      .find(_._2)
      .map(_._1)
  }

  @annotation.tailrec
  private def step(acc: Set[String], index: Int, machine: Machine): Set[String] = {
    if (index >= machine.start.length) acc
    else {
      val start = machine.start
      val applicable = machine.replacements
        .filter(r => r.from == start.slice(index, index + r.from.length))
        .map(r => start.slice(0, index) + r.to + start.slice(index + r.from.length, start.length))
      step(acc ++ applicable, index + 1, machine)
    }
  }
}
