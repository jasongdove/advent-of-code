// using java-opt -Xms64m
// using java-opt -Xmx1g

package adventofcode.year2021

import cats.effect._
import adventofcode._
import adventofcode.utils._

object Day14 extends IOApp {

  case class InsertionRule(pair: String, insert: String)

  case class Input(template: String, rules: List[InsertionRule])

  case object Input {
    def from(input: List[String]): Input = {
      val template = input.head
      val rules = input.tail.tail.map(line => {
        val split = line.split(" -> ")
        InsertionRule(split(0), split(1))
      })
      Input(template, rules)
    }
  }

  case class Context(steps: Int)

  object Runner extends Day[Input, Context, Long](2021, 14) {
    override def transformInput(lines: List[String]): Input =
      Input.from(lines)

    override def partOneContext(): Option[Context] =
      Some(Context(10))

    override def partTwoContext(): Option[Context] =
      Some(Context(40))

    override def process(input: Input, context: Option[Context]): Option[Long] =
      context.map(ctx => {
        val counts = stepAndCount(input.template, input.rules, ctx.steps)
        counts.max - counts.min
      })

    private def stepAndCount(input: String, rules: List[InsertionRule], steps: Int): List[Long] = {
      val startPairs = input
        .sliding(2, 1)
        .toList
        .frequency

      val result = (1 to steps).foldLeft(startPairs)((acc, _) => step(acc, rules))

      result
        // count all left chars
        .foldLeft(Map.empty[Char, Long]) {
          case (acc, (pair, count)) => {
            val left = pair.head
            acc.updated(left, acc.get(left).getOrElse(0L) + count)
          }
        }
        // add 1 to last char count
        .map { pair => if (pair._1 == input.last) pair._2 + 1 else pair._2 }
        .toList
    }

    private def step(pairs: Map[String, Long], rules: List[InsertionRule]): Map[String, Long] = {
      @annotation.tailrec
      def loop(q: Map[String, Long], acc: Map[String, Long]): Map[String, Long] = {
        if (q.isEmpty) acc
        else {
          val (pair, count) = q.head
          val rule = rules.find(_.pair == pair)
          val nextAcc = rule match {
            case Some(value) =>
              val toAdd = Vector(value.pair(0) + value.insert, value.insert + value.pair(1))
              val sub = acc.updated(pair, acc(pair) - count)
              toAdd.foldLeft(sub)((a, add) => a.updated(add, a.get(add).getOrElse(0L) + count))
            case None => acc
          }
          loop(q.tail, nextAcc)
        }
      }

      loop(pairs, pairs)
    }
  }

  override def run(args: List[String]): IO[ExitCode] = Runner.run(args)
}
