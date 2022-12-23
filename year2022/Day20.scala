package adventofcode.year2022

import adventofcode.Day
import cats.effect._

object Day20 extends IOApp {
  case class Number(moveOrder: Int, value: Long)

  case class Input(numbers: List[Number])

  case object Input {
    def from(lines: List[String]): Input = {
      Input(lines.zipWithIndex.map { case (line: String, index: Int) => Number(index, line.toInt) })
    }
  }

  def mix(numbers: List[Number], debug: Boolean): List[Number] = {
    val list = scala.collection.mutable.ArrayBuffer.from(numbers)
    for (moveOrder <- numbers.filter(_.value != 0).map(_.moveOrder).sorted) {
      val item = list.find(_.moveOrder == moveOrder).get
      val num = item.value
      val currentIndex = list.indexWhere(_.moveOrder == moveOrder)
      val toMove = (math.abs(num) % (list.size - 1)) * math.signum(num)
      if (toMove != 0) {
        list.remove(currentIndex)
        var targetIndex = (currentIndex + toMove)
        if (targetIndex < 0) {
          targetIndex = numbers.size - 1 - math.abs(targetIndex)
        } else if (targetIndex > list.size) {
          targetIndex = targetIndex % (list.size)
        }

        if (currentIndex != targetIndex) {
          if (targetIndex == 0) {
            targetIndex = list.size
          }
          list.insert(targetIndex.toInt, item)
        }
      }
    }
    list.toList
  }

  case class Context(solve: Input => Long)

  object Runner extends Day[Input, Context, Long](2022, 20) {
    override def transformInput(lines: List[String]): Input =
      Input.from(lines)

    override def partOneContext(): Option[Context] =
      Some(Context(input => {
        val mixed = mix(input.numbers, input.numbers.size < 100)
        val zeroIndex = mixed.indexWhere(_.value == 0)
        val one = mixed((zeroIndex + 1000) % mixed.size).value
        val two = mixed((zeroIndex + 2000) % mixed.size).value
        val three = mixed((zeroIndex + 3000) % mixed.size).value
        one + two + three
      }))

    override def partTwoContext(): Option[Context] =
      Some(Context(input => {
        val decrypted = input.numbers.map(n => Number(n.moveOrder, n.value * 811589153L))
        var mixed = decrypted
        for (_ <- 1 to 10) {
          mixed = mix(mixed, input.numbers.size < 100)
        }
        val zeroIndex = mixed.indexWhere(_.value == 0)
        val one = mixed((zeroIndex + 1000) % mixed.size).value
        val two = mixed((zeroIndex + 2000) % mixed.size).value
        val three = mixed((zeroIndex + 3000) % mixed.size).value
        one + two + three
      }))

    override def process(input: Input, context: Option[Context]): Option[Long] =
      context.map(_.solve(input))
  }

  override def run(args: List[String]): IO[ExitCode] = Runner.run(args)
}
