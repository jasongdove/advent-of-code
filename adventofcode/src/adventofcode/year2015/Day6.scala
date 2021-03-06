package adventofcode.year2015

import adventofcode.Day
import cats.effect._

import scala.util.matching.Regex

object Day6 extends IOApp {
  case class GridRange(x1: Int, y1: Int, x2: Int, y2: Int)

  sealed trait Action

  object Action {
    case object TurnOn extends Action
    case object Toggle extends Action
    case object TurnOff extends Action
  }

  case class SantaInstruction(action: Action, range: GridRange)

  object SantaInstruction {
    val pattern: Regex = "(.*) (\\d+),(\\d+) through (\\d+),(\\d+)".r
    def from(line: String): SantaInstruction = {
      val pattern(actionText, x1, y1, x2, y2) = line
      val action = actionText match {
        case "turn on"  => Action.TurnOn
        case "turn off" => Action.TurnOff
        case _          => Action.Toggle
      }
      new SantaInstruction(action, new GridRange(x1.toInt, y1.toInt, x2.toInt, y2.toInt))
    }
  }

  case class Context(turnOn: Int => Int, turnOff: Int => Int, toggle: Int => Int)

  object Runner extends Day[List[SantaInstruction], Context, Int](2015, 6) {
    override def transformInput(lines: List[String]): List[SantaInstruction] =
      lines.map(SantaInstruction.from)

    override def partOneContext(): Option[Context] =
      Some(Context(_ => 1, _ => 0, i => if (i > 0) 0 else 1))

    override def partTwoContext(): Option[Context] =
      Some(Context(i => i + 1, i => if ((i - 1) < 0) 0 else i - 1, i => i + 2))

    override def process(input: List[SantaInstruction], context: Option[Context]): Option[Int] =
      context.map { ctx =>
        val grid = Array.ofDim[Int](1000, 1000)
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

  override def run(args: List[String]): IO[ExitCode] = Runner.run(args)
}
