package adventofcode.year2020

import adventofcode.Day

sealed abstract class Day14Instruction

object Day14Instruction {
  private val maskPattern = "mask = ([X01]{36})".r
  private val assignPattern = "mem\\[(\\d+)\\] = (\\d+)".r

  case class SetMask(value: String) extends Day14Instruction
  case class Assign(address: Long, value: Long) extends Day14Instruction

  def from(line: String) = {
    line match {
      case maskPattern(mask)             => SetMask(mask)
      case assignPattern(address, value) => Assign(address.toLong, value.toLong)
    }
  }
}

case class Day14Context(process: List[Day14Instruction] => Long)

object Day14 extends Day[List[Day14Instruction], Day14Context, Long](2020, 14) {
  case class BitInstruction(bit: Int, turnOn: Boolean)

  implicit class Day14Long(long: Long) {
    def applyInstructions(bitInstructions: Iterable[BitInstruction]): Long =
      bitInstructions
        .foldLeft(BigInt(long)) {
          case (acc, BitInstruction(bit, true))  => acc.setBit(bit)
          case (acc, BitInstruction(bit, false)) => acc.clearBit(bit)
        }
        .toLong
  }

  override def transformInput(lines: List[String]): List[Day14Instruction] =
    lines.map(Day14Instruction.from)

  override def partOneContext(): Option[Day14Context] =
    Some(Day14Context(processPartOne))

  override def partTwoContext(): Option[Day14Context] =
    Some(Day14Context(processPartTwo))

  override def process(input: List[Day14Instruction], context: Option[Day14Context]): Option[Long] =
    context.map(_.process(input))

  private def processPartOne(input: List[Day14Instruction]): Long = {
    def applyMaskToValue(mask: String, value: Long): Long = {
      val bitInstructions = mask.reverse.zipWithIndex.collect {
        case (c, i) if c == '1' => BitInstruction(i, true)
        case (c, i) if c == '0' => BitInstruction(i, false)
      }

      value.applyInstructions(bitInstructions)
    }

    @annotation.tailrec
    def evaluate(mask: String, mem: Map[Long, Long], remaining: List[Day14Instruction]): Map[Long, Long] = {
      remaining match {
        case Nil => mem
        case head :: next =>
          head match {
            case Day14Instruction.SetMask(value) =>
              evaluate(value, mem, next)
            case Day14Instruction.Assign(address, value) =>
              evaluate(mask, mem.updated(address.toLong, applyMaskToValue(mask, value)), next)
          }
      }
    }

    evaluate("", Map.empty, input).map(_._2).sum
  }

  private def processPartTwo(input: List[Day14Instruction]): Long = {
    def applyMaskToAddress(mask: String, value: Long): Iterable[Long] = {
      val turnOnInstructions = mask.reverse.zipWithIndex.collect {
        case (c, i) if (c == '1') => BitInstruction(i, true)
      }
      val partiallyMaskedValue = value.applyInstructions(turnOnInstructions)

      val floatIndexes = mask.reverse.zipWithIndex.collect { case (c, i) if c == 'X' => i }
      val instructionSets = Range(0, math.pow(2, floatIndexes.length.toDouble).toInt).map { r =>
        val padded = r.toBinaryString.reverse.padTo(floatIndexes.length, '0').reverse
        padded.zipWithIndex
          .map { case (c, i) =>
            BitInstruction(floatIndexes(i), c == '1')
          }
      }

      instructionSets.map(partiallyMaskedValue.applyInstructions)
    }

    @annotation.tailrec
    def loop(mask: String, mem: Map[Long, Long], remaining: List[Day14Instruction]): Map[Long, Long] = {
      remaining match {
        case Nil => mem
        case head :: next =>
          head match {
            case Day14Instruction.SetMask(value) =>
              loop(value, mem, next)
            case Day14Instruction.Assign(address, value) =>
              val addresses = applyMaskToAddress(mask, address)
              val updatedMem = addresses.foldLeft(mem) { case (acc, address) =>
                acc.updated(address, value)
              }
              loop(mask, updatedMem, next)
          }
      }
    }
    loop("", Map.empty, input).map(_._2).sum
  }

}
