package adventofcode.year2016

import adventofcode.Day

case class Day6Context(selector: Map[Char, Int] => Char)

object Day6 extends Day[List[String], Day6Context, String](2016, 6) {

  override def transformInput(lines: List[String]): List[String] = lines

  override def partOneContext(): Option[Day6Context] =
    Some(Day6Context(_.toList.sortBy(-_._2).head._1))

  override def partTwoContext(): Option[Day6Context] =
    Some(Day6Context(_.toList.sortBy(_._2).head._1))

  override def process(input: List[String], context: Option[Day6Context]): Option[String] =
    context.map { ctx =>
      (0 to input.head.length - 1).map { position =>
        ctx.selector(input.map(_.charAt(position)).groupMapReduce(identity)(_ => 1)(_ + _))
      }.mkString
    }
}
