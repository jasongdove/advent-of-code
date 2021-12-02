package adventofcode.year2021

import adventofcode.Day
import cats.effect._

object Day2 extends IOApp {

  sealed trait Instruction {
    val num: Int
  }

  object Instruction {
    case class Down(num: Int) extends Instruction
    case class Up(num: Int) extends Instruction
    case class Forward(num: Int) extends Instruction
  }

  case class InstructionImpl(eval: (Int, Location) => Location)

  case class Context(down: InstructionImpl, up: InstructionImpl, forward: InstructionImpl)
  case class Location(horiz: Int, depth: Int, aim: Int)

  object Runner extends Day[List[Instruction], Context, Int](2021, 2) {
    override def transformInput(lines: List[String]): List[Instruction] =
      lines.map { line =>
        line.split(' ') match {
          case Array("up", i)      => Instruction.Up(i.toInt)
          case Array("down", i)    => Instruction.Down(i.toInt)
          case Array("forward", i) => Instruction.Forward(i.toInt)
        }
      }

    override def partOneContext(): Option[Context] =
      Some(
        Context(
          InstructionImpl((i, l) => Location(l.horiz, l.depth + i, l.aim)),
          InstructionImpl((i, l) => Location(l.horiz, l.depth - i, l.aim)),
          InstructionImpl((i, l) => Location(l.horiz + i, l.depth, l.aim))
        )
      )

    override def partTwoContext(): Option[Context] =
      Some(
        Context(
          InstructionImpl((i, l) => Location(l.horiz, l.depth, l.aim + i)),
          InstructionImpl((i, l) => Location(l.horiz, l.depth, l.aim - i)),
          InstructionImpl((i, l) => Location(l.horiz + i, l.depth + l.aim * i, l.aim))
        )
      )

    override def process(input: List[Instruction], context: Option[Context]): Option[Int] =
      context.map { ctx =>
        val location = input.foldLeft(Location(0, 0, 0))((l, d) => {
          d match {
            case Instruction.Down(num)    => ctx.down.eval(num, l)
            case Instruction.Up(num)      => ctx.up.eval(num, l)
            case Instruction.Forward(num) => ctx.forward.eval(num, l)
          }
        })
        location.depth * location.horiz
      }
  }

  override def run(args: List[String]): IO[ExitCode] = Runner.run(args)
}
