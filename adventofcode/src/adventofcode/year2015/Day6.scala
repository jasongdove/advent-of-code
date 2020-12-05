package adventofcode.year2015

import adventofcode.Day

case class Range(x1: Int, y1: Int, x2: Int, y2: Int)

sealed abstract class Action

object Action {
  case object TurnOn extends Action
  case object Toggle extends Action
  case object TurnOff extends Action
}

case class SantaInstruction(action: Action, range: Range)

object SantaInstruction {
  val pattern = "(.*) (\\d+),(\\d+) through (\\d+),(\\d+)".r
  def from(line: String): SantaInstruction = {
    val pattern(actionText, x1, y1, x2, y2) = line
    val action = actionText match {
      case "turn on"  => Action.TurnOn
      case "turn off" => Action.TurnOff
      case _          => Action.Toggle
    }
    new SantaInstruction(action, new Range(x1.toInt, y1.toInt, x2.toInt, y2.toInt))
  }
}

case class Day6Context(turnOn: Long => Long, turnOff: Long => Long, toggle: Long => Long)

object Day6 extends Day[List[SantaInstruction], Day6Context](2015, 6) {
  override def transformInput(lines: List[String]): List[SantaInstruction] =
    lines.map(SantaInstruction.from)

  override def partOneContext(): Option[Day6Context] =
    Some(Day6Context(_ => 1, _ => 0, i => if (i > 0) 0 else 1))

  override def partTwoContext(): Option[Day6Context] =
    Some(Day6Context(i => i + 1, i => if ((i - 1) < 0) 0 else i - 1, i => i + 2))

  override def process(input: List[SantaInstruction], context: Option[Day6Context]): Option[Long] =
    context.map { ctx =>
      val grid = Array.ofDim[Long](1000, 1000)
      input.foreach { instruction =>
        for (x <- (instruction.range.x1 to instruction.range.x2))
          for (y <- (instruction.range.y1 to instruction.range.y2))
            grid(x)(y) = instruction.action match {
              case Action.TurnOn  => ctx.turnOn(grid(x)(y))
              case Action.TurnOff => ctx.turnOff(grid(x)(y))
              case Action.Toggle  => ctx.toggle(grid(x)(y))
            }
      }
      grid.flatten.sum
    }
}
