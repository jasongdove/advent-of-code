package adventofcode.year2015

import adventofcode.Day
import cats.effect._

object Day23 extends IOApp {
  sealed trait AsmOperation

  object AsmOperation {
    case object Hlf extends AsmOperation
    case object Tpl extends AsmOperation
    case object Inc extends AsmOperation
    case object Jmp extends AsmOperation
    case object Jie extends AsmOperation
    case object Jio extends AsmOperation
  }

  case class AsmInstruction(operation: AsmOperation, register: Option[Char], operand: Option[Int])

  object AsmInstruction {
    private val halfPattern = "hlf ([ab])".r
    private val triplePattern = "tpl ([ab])".r
    private val incrementPattern = "inc ([ab])".r
    private val jumpPattern = "jmp ([\\+-]?\\d+)".r
    private val jumpEvenPattern = "jie ([ab]), ([\\+-]?\\d+)".r
    private val jumpOnePattern = "jio ([ab]), ([\\+-]?\\d+)".r

    def from(line: String): AsmInstruction =
      line match {
        case halfPattern(register)      => AsmInstruction(AsmOperation.Hlf, Some(register.head), None)
        case triplePattern(register)    => AsmInstruction(AsmOperation.Tpl, Some(register.head), None)
        case incrementPattern(register) => AsmInstruction(AsmOperation.Inc, Some(register.head), None)
        case jumpPattern(offset)        => AsmInstruction(AsmOperation.Jmp, None, Some(offset.toInt))
        case jumpEvenPattern(register, offset) =>
          AsmInstruction(AsmOperation.Jie, Some(register.head), Some(offset.toInt))
        case jumpOnePattern(register, offset) =>
          AsmInstruction(AsmOperation.Jio, Some(register.head), Some(offset.toInt))
      }
  }

  case class Context(startingRegisters: Map[Char, Int])

  def evaluate(instructions: List[AsmInstruction], startingRegisters: Map[Char, Int]): Map[Char, Int] = {
    @annotation.tailrec
    def eval(acc: Map[Char, Int], currentIndex: Int): Map[Char, Int] =
      instructions.lift(currentIndex) match {
        case Some(AsmInstruction(AsmOperation.Hlf, Some(register), None)) =>
          eval(acc.updated(register, acc.getOrElse(register, 0) / 2), currentIndex + 1)
        case Some(AsmInstruction(AsmOperation.Tpl, Some(register), None)) =>
          eval(acc.updated(register, acc.getOrElse(register, 0) * 3), currentIndex + 1)
        case Some(AsmInstruction(AsmOperation.Inc, Some(register), None)) =>
          eval(acc.updated(register, acc.getOrElse(register, 0) + 1), currentIndex + 1)
        case Some(AsmInstruction(AsmOperation.Jmp, None, Some(offset))) =>
          eval(acc, currentIndex + offset)
        case Some(AsmInstruction(AsmOperation.Jie, Some(register), Some(offset))) =>
          val o = if (acc.getOrElse(register, 0) % 2 == 0) offset else 1
          eval(acc, currentIndex + o)
        case Some(AsmInstruction(AsmOperation.Jio, Some(register), Some(offset))) =>
          val o = if (acc.getOrElse(register, 0) == 1) offset else 1
          eval(acc, currentIndex + o)
        case _ => acc
      }

    eval(startingRegisters, 0)
  }

  object Runner extends Day[List[AsmInstruction], Context, Int](2015, 23) {

    override def transformInput(lines: List[String]): List[AsmInstruction] =
      lines.map(AsmInstruction.from)

    override def partOneContext(): Option[Context] =
      Some(Context(Map.empty))

    override def partTwoContext(): Option[Context] =
      Some(Context(Map('a' -> 1)))

    override def process(input: List[AsmInstruction], context: Option[Context]): Option[Int] =
      context.flatMap { ctx =>
        evaluate(input, ctx.startingRegisters).get('b')
      }
  }

  override def run(args: List[String]): IO[ExitCode] = Runner.run(args)
}
