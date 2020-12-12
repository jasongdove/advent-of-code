package adventofcode.year2020

import adventofcode.{Day, Point}

case class Day12Instruction(action: String, value: Int)

object Day12Instruction {
  private val instructionPattern = "([NSEWLRF])(\\d+)".r

  def from(line: String): Day12Instruction = {
    val instructionPattern(action, valueString) = line
    val value = valueString.toInt
    Day12Instruction(action, value)
  }
}

case class Day12Context(process: List[Day12Instruction] => Int)

object Day12 extends Day[List[Day12Instruction], Day12Context, Int](2020, 12) {
  override def transformInput(lines: List[String]): List[Day12Instruction] =
    lines.map(Day12Instruction.from)

  override def partOneContext(): Option[Day12Context] =
    Some(Day12Context(processPartOne))

  override def partTwoContext(): Option[Day12Context] =
    Some(Day12Context(processPartTwo))

  override def process(input: List[Day12Instruction], context: Option[Day12Context]): Option[Int] =
    context.map(_.process(input))

  private def processPartOne(instructions: List[Day12Instruction]): Int = {
    def getNextLocation(current: Point, facing: Int, action: String, distance: Int) = {
      val direction = action match {
        case "F" if facing == 0 => "E"
        case "F" if facing == 1 => "S"
        case "F" if facing == 2 => "W"
        case "F" if facing == 3 => "N"
        case _                  => action
      }
      direction match {
        case "N" => Point(current.x, current.y - distance)
        case "S" => Point(current.x, current.y + distance)
        case "E" => Point(current.x + distance, current.y)
        case "W" => Point(current.x - distance, current.y)
        case _   => current
      }
    }

    @annotation.tailrec
    def loop(current: Point, facing: Int, remaining: List[Day12Instruction]): Point =
      remaining match {
        case Nil => current
        case Day12Instruction(action, value) :: tail =>
          val nextLocation = getNextLocation(current, facing, action, value)
          val nextFacing = action match {
            case "R" => (facing + (value / 90)) % 4
            case "L" => (facing + 4 - (value / 90)) % 4
            case _   => facing
          }
          loop(nextLocation, nextFacing, tail)
      }

    loop(Point(0, 0), 0, instructions).distanceFromOrigin
  }

  private def processPartTwo(instructions: List[Day12Instruction]): Int = {
    def getNextWaypoint(waypoint: Point, direction: String, distance: Int): Point = {
      def rotate(angle: Int): Point = {
        angle match {
          case 90 | -270  => Point(-waypoint.y, waypoint.x)
          case 180 | -180 => Point(-waypoint.x, -waypoint.y)
          case 270 | -90  => Point(waypoint.y, -waypoint.x)
        }
      }

      direction match {
        case "N" => Point(waypoint.x, waypoint.y - distance)
        case "S" => Point(waypoint.x, waypoint.y + distance)
        case "E" => Point(waypoint.x + distance, waypoint.y)
        case "W" => Point(waypoint.x - distance, waypoint.y)
        case "R" => rotate(distance)
        case "L" => rotate(-distance)
        case _   => waypoint
      }
    }

    @annotation.tailrec
    def loop(current: Point, waypoint: Point, remaining: List[Day12Instruction]): Point =
      remaining match {
        case Nil => current
        case Day12Instruction(action, value) :: tail =>
          val nextLocation = action match {
            case "F" => Point(current.x + waypoint.x * value, current.y + waypoint.y * value)
            case _   => current
          }
          val nextWaypoint = getNextWaypoint(waypoint, action, value)
          loop(nextLocation, nextWaypoint, tail)
      }

    loop(Point(0, 0), Point(10, -1), instructions).distanceFromOrigin
  }
}
