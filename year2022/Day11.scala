//> using javaOpt "-Xms64m"
//> using javaOpt "-Xmx1g"

package adventofcode.year2022

import adventofcode.{Day, Grid, GridLocation}
import cats.effect._

object Day11 extends IOApp {
  case class Monkey(
    num: Int,
    items: List[Long],
    operation: Long => Long,
    test: Long,
    trueTarget: Int,
    falseTarget: Int,
    inspectionCount: Long
  ) {
    def done(): Monkey = Monkey(num, List.empty, operation, test, trueTarget, falseTarget, inspectionCount + items.size)
    def grab(item: Long): Monkey = Monkey(num, items :+ item, operation, test, trueTarget, falseTarget, inspectionCount)
    def getTarget(item: Long): Int = if (item % test == 0) trueTarget else falseTarget
    def status: String = s"Monkey ${num}: ${items.mkString(", ")}"
    def inspectionStatus: String = s"Monkey ${num} inspected items ${inspectionCount} times."
  }

  case object Monkey {
    private val multiplyPattern = "\\* (\\d+)".r
    private val addPattern = "\\+ (\\d+)".r

    def from(lines: List[String]): Monkey = {
      val num = lines.head.slice(7, lines.head.length() - 1).toInt
      val items = lines(1).drop(18).split(", ").map(_.toLong).toList
      val opText = lines(2).drop(23)
      val operation: Long => Long = opText match {
        case multiplyPattern(v) => _ * v.toLong
        case addPattern(v)      => _ + v.toLong
        case "* old"            => old: Long => old * old
        case "+ old"            => old: Long => old + old
      }
      val modBy = lines(3).drop(21).toLong
      val trueTarget = lines(4).drop(29).trim().toInt
      val falseTarget = lines(5).drop(29).trim().toInt
      Monkey(num, items, operation, modBy, trueTarget, falseTarget, 0)
    }
  }

  def round(monkeys: List[Monkey], worry: Long => Long): List[Monkey] = {
    @annotation.tailrec
    def loop(currentIndex: Int, acc: List[Monkey]): List[Monkey] =
      if (currentIndex == acc.length) acc
      else {
        var nextAcc = acc
        val currentMonkey = acc(currentIndex)
        for (toInspect <- currentMonkey.items) {
          var nextLevel = currentMonkey.operation(toInspect)
          nextLevel = worry(nextLevel)
          val target = currentMonkey.getTarget(nextLevel)
          nextAcc = nextAcc.updated(target, nextAcc(target).grab(nextLevel))
        }
        loop(currentIndex + 1, nextAcc.updated(currentIndex, currentMonkey.done()))
      }

    loop(0, monkeys)
  }

  case class Context(rounds: Int, worry: List[Monkey] => Long => Long) {
    def solve(monkeys: List[Monkey]): Long = {
      val worryFn = worry(monkeys)

      @annotation.tailrec
      def loop(roundsLeft: Int, acc: List[Monkey]): List[Monkey] =
        if (roundsLeft == 0) acc
        else loop(roundsLeft - 1, round(acc, worryFn))

      loop(rounds, monkeys).map(_.inspectionCount).sorted.reverse.take(2).product
    }
  }

  object Runner extends Day[List[Monkey], Context, Long](2022, 11) {
    override def transformInput(lines: List[String]): List[Monkey] =
      lines.filter(_.nonEmpty).sliding(6, 6).map(Monkey.from).toList

    override def partOneContext(): Option[Context] =
      Some(Context(20, _ => _ / 3))

    override def partTwoContext(): Option[Context] =
      Some(Context(10_000, monkeys => _ % monkeys.map(_.test).product))

    override def process(input: List[Monkey], context: Option[Context]): Option[Long] =
      context.map(_.solve(input))
  }

  override def run(args: List[String]): IO[ExitCode] = Runner.run(args)
}
