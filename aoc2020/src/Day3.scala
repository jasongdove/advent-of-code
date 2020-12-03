package aoc2020

import cats.effect._

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

object Day3 extends IOApp {
  override def run(args: List[String]): IO[ExitCode] = {
    for {
      input <- readInput("day3.txt")
      result <- IO(search(input, 3, 1))
      _ <- printResult(result)
      result <- IO(multiSearch(input))
      _ <- printResult(result)
    } yield ExitCode.Success
  }

  def readInput(resourceName: String): IO[Map] =
    IO(Map.from(os.read(os.resource / resourceName).split("\n").toList))

  def search(map: Map, slopeRight: Int, slopeDown: Int): Long = {
    val height = map.rows.length
    val width = map.rows(0).squares.length
    @annotation.tailrec
    def inner(acc: Long, x: Int, y: Int): Long =
      if (y >= height) acc
      else inner(acc + map.rows(y).squares(x).score, (x + slopeRight) % width, y + slopeDown)
    inner(0, 0, 0)
  }

  def multiSearch(map: Map): Long = {
    val one = search(map, 1, 1)
    val two = search(map, 3, 1)
    val three = search(map, 5, 1)
    val four = search(map, 7, 1)
    val five = search(map, 1, 2)
    one * two * three * four * five
  }

  def printResult(result: Long): IO[Unit] =
    IO(println(s"found $result trees"))
}
