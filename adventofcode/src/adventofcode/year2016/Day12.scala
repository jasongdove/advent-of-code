package adventofcode.year2016

import adventofcode.Day
import cats.effect._

object Day12 extends IOApp {
  sealed trait AsmInstruction

  object AsmInstruction {
    case class CpyReg(source: Char, dest: Char) extends AsmInstruction
    case class CpyLit(value: Int, dest: Char) extends AsmInstruction
    case class Inc(dest: Char) extends AsmInstruction
    case class Dec(dest: Char) extends AsmInstruction
    case class JnzReg(register: Char, offset: Int) extends AsmInstruction
    case class JnzLit(value: Int, offset: Int) extends AsmInstruction

    private val copyRegPattern = "cpy ([abcd]) ([abcd])".r
    private val copyLitPattern = "cpy (\\d+) ([abcd])".r
    private val incrementPattern = "inc ([abcd])".r
    private val decrementPattern = "dec ([abcd])".r
    private val jumpNotZeroRegPattern = "jnz ([abcd]) ([\\+-]?\\d+)".r
    private val jumpNotZeroLitPattern = "jnz (\\d+) ([\\+-]?\\d+)".r

    def from(line: String): AsmInstruction =
      line match {
        case copyRegPattern(source, dest)            => CpyReg(source.head, dest.head)
        case copyLitPattern(value, dest)             => CpyLit(value.toInt, dest.head)
        case incrementPattern(dest)                  => Inc(dest.head)
        case decrementPattern(dest)                  => Dec(dest.head)
        case jumpNotZeroRegPattern(register, offset) => JnzReg(register.head, offset.toInt)
        case jumpNotZeroLitPattern(value, offset)    => JnzLit(value.toInt, offset.toInt)
      }
  }

  case class Context(startingRegisters: Map[Char, Int])

  def evaluate(instructions: List[AsmInstruction], startingRegisters: Map[Char, Int]): Map[Char, Int] = {
    import AsmInstruction._

    @annotation.tailrec
    def eval(acc: Map[Char, Int], currentIndex: Int): Map[Char, Int] =
      instructions.lift(currentIndex) match {
        case Some(CpyReg(source, dest)) => eval(acc.updated(dest, acc.getOrElse(source, 0)), currentIndex + 1)
        case Some(CpyLit(value, dest))  => eval(acc.updated(dest, value), currentIndex + 1)
        case Some(Inc(dest))            => eval(acc.updated(dest, acc.getOrElse(dest, 0) + 1), currentIndex + 1)
        case Some(Dec(dest))            => eval(acc.updated(dest, acc.getOrElse(dest, 0) - 1), currentIndex + 1)
        case Some(JnzReg(register, offset)) =>
          val o = if (acc.getOrElse(register, 0) == 0) 1 else offset
          eval(acc, currentIndex + o)
        case Some(JnzLit(value, offset)) =>
          val o = if (value == 0) 1 else offset
          eval(acc, currentIndex + o)
        case _ => acc
      }

    eval(startingRegisters, 0)
  }

  object Runner extends Day[List[AsmInstruction], Context, Int](2016, 12) {
    override def transformInput(lines: List[String]): List[AsmInstruction] =
      lines.map(AsmInstruction.from)

    override def partOneContext(): Option[Context] =
      Some(Context(Map.empty))

    override def partTwoContext(): Option[Context] =
      Some(Context(Map('c' -> 1)))

    override def process(input: List[AsmInstruction], context: Option[Context]): Option[Int] =
      context.flatMap { ctx =>
        evaluate(input, ctx.startingRegisters).get('a')
      }
  }

  override def run(args: List[String]): IO[ExitCode] = Runner.run(args)
}
