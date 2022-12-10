package adventofcode.year2022

import adventofcode.{Day, Grid, GridLocation}
import cats.effect._

object Day10 extends IOApp {

  sealed trait AsmInstruction

  object AsmInstruction {
    case class Addx(value: Int) extends AsmInstruction
    case class Noop() extends AsmInstruction

    private val addxPattern = "addx ([\\+-]?\\d+)".r
    private val noopPattern = "noop".r

    def from(line: String): List[AsmInstruction] =
      line match {
        case addxPattern(value) => List(Noop(), Addx(value.toInt))
        case _                  => List(Noop())
      }
  }

  def evaluate(instructions: List[AsmInstruction], checkStrength: Int => Boolean): List[Int] = {
    import AsmInstruction._

    @annotation.tailrec
    def eval(acc: List[Int], x: Int, cycle: Int, currentIndex: Int): List[Int] = {
      val nextAcc = if (checkStrength(cycle)) acc :+ (cycle * x) else acc
      instructions.lift(currentIndex) match {
        case Some(Addx(value)) => eval(nextAcc, x + value, cycle + 1, currentIndex + 1)
        case Some(Noop())      => eval(nextAcc, x, cycle + 1, currentIndex + 1)
        case _                 => acc
      }
    }

    eval(List.empty, 1, 1, 0)
  }

  def evaluate2(instructions: List[AsmInstruction], lineBreak: Int => Boolean): Int = {
    import AsmInstruction._

    @annotation.tailrec
    def eval(x: Int, cycle: Int, currentIndex: Int): Int = {
      if (currentIndex < instructions.size) {
        val index = cycle % 40 - 1
        print(if (math.abs(x - index) <= 1) '#' else '.')
        if (lineBreak(cycle)) {
          println
        }
      }
      instructions.lift(currentIndex) match {
        case Some(Addx(value)) => eval(x + value, cycle + 1, currentIndex + 1)
        case Some(Noop())      => eval(x, cycle + 1, currentIndex + 1)
        case _                 => 0
      }
    }

    eval(1, 1, 0)
  }

  case class Context(solve: List[AsmInstruction] => Int)

  object Runner extends Day[List[AsmInstruction], Context, Int](2022, 10) {
    override def transformInput(lines: List[String]): List[AsmInstruction] =
      lines.flatMap(AsmInstruction.from)

    override def partOneContext(): Option[Context] =
      Some(Context(input => evaluate(input, i => (i - 20) % 40 == 0).sum))

    override def partTwoContext(): Option[Context] =
      Some(Context(input => evaluate2(input, i => i % 40 == 0)))

    override def process(input: List[AsmInstruction], context: Option[Context]): Option[Int] =
      context.map(_.solve(input))
  }

  override def run(args: List[String]): IO[ExitCode] = Runner.run(args)
}
