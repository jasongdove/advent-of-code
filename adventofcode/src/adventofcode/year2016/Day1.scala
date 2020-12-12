package adventofcode.year2016

import adventofcode.{Day, Point}

case class Day1Context(process: List[String] => Option[Int])

object Day1 extends Day[List[String], Day1Context, Int](2016, 1) {
  override def transformInput(lines: List[String]): List[String] =
    lines.mkString.trim.split(", ").toList

  override def partOneContext(): Option[Day1Context] =
    Some(Day1Context(processPartOne))

  override def partTwoContext(): Option[Day1Context] =
    Some(Day1Context(processPartTwo))

  override def process(input: List[String], context: Option[Day1Context]): Option[Int] =
    context.flatMap(_.process(input))

  private def processPartOne(instructions: List[String]): Option[Int] =
    walk(instructions).lastOption.map(_.distanceFromOrigin)

  private def processPartTwo(instructions: List[String]): Option[Int] = {
    @annotation.tailrec
    def visitedTwice(acc: Set[Point], remaining: List[Point]): Option[Point] = {
      remaining match {
        case Nil => None
        case head :: next =>
          if (acc.contains(head)) Some(head)
          else visitedTwice(acc + head, next)
      }
    }

    val visited = walk(instructions)
    visitedTwice(Set.empty, visited.toList).map(_.distanceFromOrigin)
  }

  private def walk(instructions: List[String]): Vector[Point] = {
    val instructionPattern = "([RL])(\\d+)".r

    @annotation.tailrec
    def visited(acc: Vector[Point], current: Point, facing: Int, remaining: List[String]): Vector[Point] = {
      remaining match {
        case Nil => acc
        case head :: tail =>
          val instructionPattern(direction, distanceString) = head
          val distance = distanceString.toInt
          val newFacing = (if (direction == "R") facing + 1 else facing + 3) % 4
          val newPoints = (1 to distance).map { d =>
            newFacing match {
              case 0 => Point(current.x, current.y - d)
              case 1 => Point(current.x + d, current.y)
              case 2 => Point(current.x, current.y + d)
              case 3 => Point(current.x - d, current.y)
            }
          }
          visited(acc ++ newPoints, newPoints.last, newFacing, tail)
      }
    }

    visited(Vector.empty, Point(0, 0), 0, instructions)
  }
}
