package adventofcode.year2020

import adventofcode.Day

sealed abstract class MapSquare(val score: Long) extends Product with Serializable

object MapSquare {
  final case object OpenSquare extends MapSquare(0)
  final case object Tree extends MapSquare(1)
}

case class MapRow(squares: List[MapSquare])
case class SkiMap(rows: List[MapRow])

object SkiMap {
  def from(lines: List[String]): SkiMap = {
    SkiMap(lines.map(row))
  }

  def row(line: String): MapRow = {
    MapRow(line.map(square).toList)
  }

  def square(char: Char): MapSquare =
    char match {
      case '#' => MapSquare.Tree
      case _   => MapSquare.OpenSquare
    }
}

case class Slope(right: Int, down: Int)
case class Day3Context(slopes: List[Slope])

object Day3 extends Day[SkiMap, Day3Context, Long](2020, 3) {
  override def transformInput(lines: List[String]): SkiMap =
    SkiMap.from(lines)

  override def partOneContext(): Option[Day3Context] =
    Some(Day3Context(List(Slope(3, 1))))

  override def partTwoContext(): Option[Day3Context] =
    Some(Day3Context(List(Slope(1, 1), Slope(3, 1), Slope(5, 1), Slope(7, 1), Slope(1, 2))))

  override def process(map: SkiMap, context: Option[Day3Context]): Option[Long] =
    context.map { ctx =>
      ctx.slopes.map(slope => countTrees(map, slope)).product
    }

  private def countTrees(map: SkiMap, slope: Slope) = {
    val height = map.rows.length
    val width = map.rows.head.squares.length

    @annotation.tailrec
    def inner(acc: Long, x: Int, y: Int): Long =
      if (y >= height) acc
      else inner(acc + map.rows(y).squares(x).score, (x + slope.right) % width, y + slope.down)

    inner(0, 0, 0)
  }
}
