package adventofcode.year2020

import adventofcode.Day

sealed trait AsmOperation

object AsmOperation {
  case object Nop extends AsmOperation
  case object Acc extends AsmOperation
  case object Jmp extends AsmOperation

  def from(op: String): AsmOperation =
    op match {
      case "acc" => Acc
      case "jmp" => Jmp
      case _     => Nop
    }
}

case class AsmInstruction(operation: AsmOperation, operand: Int)

case class Day8Context(
  transform: List[AsmInstruction] => List[List[AsmInstruction]],
  aggregate: List[(Boolean, Int)] => Option[Int]
)

object Day8 extends Day[List[AsmInstruction], Day8Context, Int](2020, 8) {

  override def transformInput(lines: List[String]): List[AsmInstruction] =
    lines.map { line =>
      val split = line.split(" ")
      AsmInstruction(AsmOperation.from(split(0)), split(1).toInt)
    }

  override def partOneContext(): Option[Day8Context] =
    Some(Day8Context(List(_), r => Some(r.head._2)))

  override def partTwoContext(): Option[Day8Context] =
    Some(Day8Context(transformInputForPartTwo, _.find(_._1).map(_._2)))

  override def process(input: List[AsmInstruction], context: Option[Day8Context]): Option[Int] =
    context.flatMap { ctx =>
      ctx.aggregate(ctx.transform(input).map(terminates))
    }

  def terminates(instructions: List[AsmInstruction]): (Boolean, Int) = {
    @annotation.tailrec
    def eval(acc: Int, visited: List[Int], currentIndex: Int): (Boolean, Int) =
      instructions.lift(currentIndex) match {
        case None => (true, acc)
        case Some(currentInstruction) =>
          if (visited.contains(currentIndex)) (false, acc)
          else
            currentInstruction.operation match {
              case AsmOperation.Nop => eval(acc, currentIndex +: visited, currentIndex + 1)
              case AsmOperation.Acc => eval(acc + currentInstruction.operand, currentIndex +: visited, currentIndex + 1)
              case AsmOperation.Jmp => eval(acc, currentIndex +: visited, currentIndex + currentInstruction.operand)
            }
      }

    eval(0, List.empty, 0)
  }

  private def transformInputForPartTwo(input: List[AsmInstruction]): List[List[AsmInstruction]] =
    input.indices.foldLeft(List[List[AsmInstruction]]()) { case (acc, i) =>
      input(i) match {
        case AsmInstruction(AsmOperation.Nop, operand) =>
          input.updated(i, AsmInstruction(AsmOperation.Jmp, operand)) +: acc
        case AsmInstruction(AsmOperation.Acc, _) => acc
        case AsmInstruction(AsmOperation.Jmp, operand) =>
          input.updated(i, AsmInstruction(AsmOperation.Nop, operand)) +: acc
      }
    }
}
