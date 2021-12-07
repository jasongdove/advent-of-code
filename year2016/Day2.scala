package adventofcode.year2016

import adventofcode.{Day, GridLocation}
import cats.effect._

object Day2 extends IOApp {
  case class Keypad(startLocation: GridLocation, data: List[List[Option[Char]]]) {}

  case class Context(keypad: Keypad)

  object Runner extends Day[List[String], Context, String](2016, 2) {
    val partOneData = List(
      List(Some('1'), Some('2'), Some('3')),
      List(Some('4'), Some('5'), Some('6')),
      List(Some('7'), Some('8'), Some('9'))
    )

    val partTwoData = List(
      List(None, None, Some('1'), None, None),
      List(None, Some('2'), Some('3'), Some('4'), None),
      List(Some('5'), Some('6'), Some('7'), Some('8'), Some('9')),
      List(None, Some('A'), Some('B'), Some('C'), None),
      List(None, None, Some('D'), None, None)
    )

    override def transformInput(lines: List[String]): List[String] = lines

    override def partOneContext(): Option[Context] =
      Some(Context(Keypad(GridLocation(1, 1), partOneData)))

    override def partTwoContext(): Option[Context] =
      Some(Context(Keypad(GridLocation(0, 2), partTwoData)))

    override def process(input: List[String], context: Option[Context]): Option[String] =
      context.map { ctx =>
        val max = ctx.keypad.data.length
        def move(l: GridLocation, dr: Int, dc: Int) = {
          val nl = GridLocation(l.row + dr, l.col + dc)
          val onGrid = nl.row >= 0 && nl.col >= 0 && nl.row < max && nl.col < max
          if (onGrid && ctx.keypad.data(nl.row)(nl.col).isDefined) nl else l
        }

        input
          .foldLeft(("", ctx.keypad.startLocation)) { case ((code, location), line) =>
            val newLocation = line.foldLeft(location) { case (l, instruction) =>
              instruction match {
                case 'U' => move(l, -1, 0)
                case 'L' => move(l, 0, -1)
                case 'D' => move(l, 1, 0)
                case 'R' => move(l, 0, 1)
              }
            }
            (code ++ ctx.keypad.data(newLocation.row)(newLocation.col), newLocation)
          }
          ._1
      }
  }

  override def run(args: List[String]): IO[ExitCode] = Runner.run(args)
}
