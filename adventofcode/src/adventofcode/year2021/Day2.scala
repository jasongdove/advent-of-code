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

    def from(line: String): Instruction =
      line.split(' ') match {
        case Array("up", i)      => Instruction.Up(i.toInt)
        case Array("down", i)    => Instruction.Down(i.toInt)
        case Array("forward", i) => Instruction.Forward(i.toInt)
      }
  }

  case class Context(process: (Location, Instruction) => Location)
  case class Location(horiz: Int, depth: Int, aim: Int)

  object Runner extends Day[List[Instruction], Context, Int](2021, 2) {
    override def transformInput(lines: List[String]): List[Instruction] =
      lines.map(Instruction.from)

    override def partOneContext(): Option[Context] =
      Some(
        Context((loc: Location, instruction: Instruction) => {
          instruction match {
            case Instruction.Down(num)    => loc.copy(depth = loc.depth + num)
            case Instruction.Up(num)      => loc.copy(depth = loc.depth - num)
            case Instruction.Forward(num) => loc.copy(horiz = loc.horiz + num)
          }
        })
      )

    override def partTwoContext(): Option[Context] =
      Some(
        Context((loc: Location, instruction: Instruction) => {
          instruction match {
            case Instruction.Down(num)    => loc.copy(aim = loc.aim + num)
            case Instruction.Up(num)      => loc.copy(aim = loc.aim - num)
            case Instruction.Forward(num) => loc.copy(horiz = loc.horiz + num, depth = loc.depth + loc.aim * num)
          }
        })
      )

    override def process(input: List[Instruction], context: Option[Context]): Option[Int] =
      context.map { ctx =>
        val location = input.foldLeft(Location(0, 0, 0))(ctx.process)
        location.depth * location.horiz
      }
  }

  override def run(args: List[String]): IO[ExitCode] = Runner.run(args)
}
