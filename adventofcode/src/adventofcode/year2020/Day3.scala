package adventofcode.year2020

import adventofcode.Day

sealed abstract class MapSquare(val score: Long) extends Product with Serializable

object MapSquare {
  final case object OpenSquare extends MapSquare(0)
  final case object Tree extends MapSquare(1)
}

case class MapRow(squares: List[MapSquare])
case class Map(rows: List[MapRow])

object Map {
  def from(lines: List[String]): Map = {
    new Map(lines.map(row(_)))
  }

  def row(line: String): MapRow = {
    new MapRow(line.map(square(_)).toList)
  }

  def square(char: Char): MapSquare =
    char match {
      case '#' => MapSquare.Tree
      case _   => MapSquare.OpenSquare
    }
}

case class Slope(right: Int, down: Int)
case class Day3Context(slopes: List[Slope])

object Day3 extends Day[Map, Day3Context, Long](2020, 3) {
  override def transformInput(lines: List[String]): Map =
    Map.from(lines)

  override def partOneContext(): Option[Day3Context] =
    Some(Day3Context(List(Slope(3, 1))))

  override def partTwoContext(): Option[Day3Context] =
    Some(Day3Context(List(Slope(1, 1), Slope(3, 1), Slope(5, 1), Slope(7, 1), Slope(1, 2))))

  override def process(map: Map, context: Option[Day3Context]): Option[Long] =
    context.map { ctx =>
      ctx.slopes.map(slope => countTrees(map, slope)).reduce(_ * _)
    }

  private def countTrees(map: Map, slope: Slope) = {
    val height = map.rows.length
    val width = map.rows(0).squares.length

    @annotation.tailrec
    def inner(acc: Long, x: Int, y: Int): Long =
      if (y >= height) acc
      else inner(acc + map.rows(y).squares(x).score, (x + slope.right) % width, y + slope.down)

    inner(0, 0, 0)
  }
}
