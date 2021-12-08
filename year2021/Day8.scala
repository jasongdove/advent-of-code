package adventofcode.year2021

import adventofcode.Day
import cats.effect._

object Day8 extends IOApp {

  case class Entry(signalPatterns: List[String], outputValues: List[String]) {
    val ez = outputValues.count(v => List(2, 3, 4, 7).contains(v.length()))

    val output = {
      val one = signalPatterns.find(_.length == 2).head.toSet
      val four = signalPatterns.find(_.length == 4).head.toSet
      val seven = signalPatterns.find(_.length == 3).head.toSet
      val eight = signalPatterns.find(_.length == 7).head.toSet

      val has_five = signalPatterns.filter(_.length == 5).map(_.toSet).toList
      val has_six = signalPatterns.filter(_.length == 6).map(_.toSet).toList

      val zero = has_six.find(s => one.forall(c => s.contains(c)) && four.exists(c => !s.toSet.contains(c))).head.toSet
      val seg_d = four.find(c => !zero.contains(c)).head

      val six = has_six.filterNot(_ == zero).find(s => four.exists(c => !s.contains(c))).head.toSet
      val nine = has_six.filterNot(s => s == zero || s == six).head.toSet

      val seg_c = one.find(c => !six.contains(c)).head
      val seg_e = eight.find(c => !nine.contains(c)).head

      val two = has_five.find(s => s.contains(seg_c) && s.contains(seg_e)).head.toSet
      val five = has_five.filterNot(_ == two).find(s => !s.contains(seg_c)).head.toSet
      val three = has_five.filterNot(s => s == two || s == five).head.toSet

      outputValues
        .map { v =>
          v.toSet match {
            case `zero`  => '0'
            case `one`   => '1'
            case `two`   => '2'
            case `three` => '3'
            case `four`  => '4'
            case `five`  => '5'
            case `six`   => '6'
            case `seven` => '7'
            case `eight` => '8'
            case _       => '9'
          }
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
