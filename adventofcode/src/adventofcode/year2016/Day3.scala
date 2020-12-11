package adventofcode.year2016

import adventofcode.Day

case class Day3Context(inputTransform: List[List[Int]] => List[List[Int]])

object Day3 extends Day[List[List[Int]], Day3Context, Int](2016, 3) {
  private val inputPattern = "\\s+(\\d+)\\s+(\\d+)\\s+(\\d+)".r
  override def transformInput(lines: List[String]): List[List[Int]] = {
    lines.map { line =>
      val inputPattern(one, two, three) = line
      List(one.toInt, two.toInt, three.toInt)
    }
  }

  override def partOneContext(): Option[Day3Context] =
    Some(Day3Context(identity))

  override def partTwoContext(): Option[Day3Context] =
    Some(Day3Context(vertical))

  override def process(input: List[List[Int]], context: Option[Day3Context]): Option[Int] =
    context.map { ctx =>
      val transformed = ctx.inputTransform(input)
      transformed.count { triangle =>
        triangle(0) + triangle(1) > triangle(2) &&
        triangle(1) + triangle(2) > triangle(0) &&
        triangle(0) + triangle(2) > triangle(1)
      }
    }

  private def vertical(input: List[List[Int]]): List[List[Int]] =
    input
      .sliding(3, 3)
      .flatMap { row =>
        val one = List(row(0)(0), row(1)(0), row(2)(0))
        val two = List(row(0)(1), row(1)(1), row(2)(1))
        val three = List(row(0)(2), row(1)(2), row(2)(2))
        List(one, two, three)
      }
      .toList
}
