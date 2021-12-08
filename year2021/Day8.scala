package adventofcode.year2021

import adventofcode.Day
import cats.effect._

object Day8 extends IOApp {

  case class Entry(signalPatterns: List[String], outputValues: List[String]) {
    val ez = outputValues.count(v => List(2, 3, 4, 7).contains(v.length()))

    val output = {
      val one = signalPatterns.find(_.length == 2).head
      val four = signalPatterns.find(_.length == 4).head
      val seven = signalPatterns.find(_.length == 3).head
      val eight = signalPatterns.find(_.length == 7).head

      val has_five = signalPatterns.filter(_.length == 5).toList
      val has_six = signalPatterns.filter(_.length == 6).toList

      val zero = has_six.find(s => one.forall(c => s.contains(c)) && four.exists(c => !s.toSet.contains(c))).head
      val seg_d = four.find(c => !zero.contains(c)).head

      val six = has_six.filterNot(_ == zero).find(s => four.exists(c => !s.contains(c))).head
      val nine = has_six.filterNot(s => s == zero || s == six).head

      val seg_c = one.find(c => !six.contains(c)).head
      val seg_e = eight.find(c => !nine.contains(c)).head

      val two = has_five.find(s => s.contains(seg_c) && s.contains(seg_e)).head
      val five = has_five.filterNot(_ == two).find(s => !s.contains(seg_c)).head
      val three = has_five.filterNot(s => s == two || s == five).head

      outputValues
        .map { v =>
          val vset = v.toSet
          if (vset == zero.toSet) '0'
          else if (vset == one.toSet) '1'
          else if (vset == two.toSet) '2'
          else if (vset == three.toSet) '3'
          else if (vset == four.toSet) '4'
          else if (vset == five.toSet) '5'
          else if (vset == six.toSet) '6'
          else if (vset == seven.toSet) '7'
          else if (vset == eight.toSet) '8'
          else '9'
        }
        .mkString
        .toInt
    }
  }

  case class Input(entries: List[Entry])

  case object Input {
    def from(input: List[String]): Input = {
      val entries = input.map { line =>
        val parts = line.split('|').toList
        Entry(
          parts.head.split(' ').map(_.trim).toList,
          parts.last.split(' ').map(_.trim).filterNot(_.isEmpty).toList
        )
      }
      Input(entries)
    }
  }

  case class Context(map: Entry => Int)

  object Runner extends Day[Input, Context, Int](2021, 8) {
    override def transformInput(lines: List[String]): Input =
      Input.from(lines)

    override def partOneContext(): Option[Context] =
      Some(Context(_.ez))

    override def partTwoContext(): Option[Context] =
      Some(Context(_.output))

    override def process(input: Input, context: Option[Context]): Option[Int] =
      context.map(ctx => input.entries.map(ctx.map).sum)
  }

  override def run(args: List[String]): IO[ExitCode] = Runner.run(args)
}
