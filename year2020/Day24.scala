//> using javaOpt "-Xms64m"
//> using javaOpt "-Xmx1g"

package adventofcode.year2020

import adventofcode.Day
import adventofcode.utils._
import cats.effect._

object Day24 extends IOApp {
  sealed trait Direction
  case object East extends Direction
  case object SouthEast extends Direction
  case object SouthWest extends Direction
  case object West extends Direction
  case object NorthWest extends Direction
  case object NorthEast extends Direction

  case class Tile(x: Int, y: Int, z: Int) {
    lazy val neighbors = List(
      move(SouthEast),
      move(SouthWest),
      move(NorthEast),
      move(NorthWest),
      move(East),
      move(West)
    )

    def move(direction: Direction): Tile =
      direction match {
        case SouthEast => Tile(x, y - 1, z + 1)
        case SouthWest => Tile(x - 1, y, z + 1)
        case NorthEast => Tile(x + 1, y, z - 1)
        case NorthWest => Tile(x, y + 1, z - 1)
        case East      => Tile(x + 1, y - 1, z)
        case West      => Tile(x - 1, y + 1, z)
      }

    def shouldFlipToBlack(blackTiles: List[Tile]): Boolean = {
      val adjacentBlackTiles = blackTiles intersect neighbors
      adjacentBlackTiles.size == 2
    }

    def shouldFlipToWhite(blackTiles: List[Tile]): Boolean = {
      val adjacentBlackTiles = blackTiles intersect neighbors
      adjacentBlackTiles.isEmpty || adjacentBlackTiles.size > 2
    }
  }

  case class Context(process: List[Tile] => Int)

  private def processPartOne(blackTiles: List[Tile]): Int =
    blackTiles.length

  private def processPartTwo(blackTiles: List[Tile]): Int = {
    Iterator.iterate(blackTiles)(iterate).drop(100).next().length
  }

  private def parseDirectionList(line: String): List[Direction] = {
    @annotation.tailrec
    def loop(acc: Vector[Direction], remaining: List[Char]): List[Direction] = {
      remaining match {
        case 's' :: 'e' :: next => loop(acc :+ SouthEast, next)
        case 's' :: 'w' :: next => loop(acc :+ SouthWest, next)
        case 'n' :: 'e' :: next => loop(acc :+ NorthEast, next)
        case 'n' :: 'w' :: next => loop(acc :+ NorthWest, next)
        case 'e' :: next        => loop(acc :+ East, next)
        case 'w' :: next        => loop(acc :+ West, next)
        case _                  => acc.toList
      }
    }

    loop(Vector.empty, line.toList)
  }

  private def followDirections(directions: List[Direction]): Tile =
    directions.foldLeft(Tile(0, 0, 0)) { case (tile, direction) => tile.move(direction) }

  private def tilesToFlip(tiles: List[Tile]): List[Tile] =
    tiles
      .frequency
      .collect {
        case (tile, size) if size % 2 == 1 => tile
      }
      .toList

  private def iterate(tiles: List[Tile]): List[Tile] = {
    val allTilesToCheck = tiles.flatMap(_.neighbors).distinct

    val blackTiles = tiles
    val whiteTiles = allTilesToCheck diff blackTiles

    val unchangedBlackTiles = blackTiles.filterNot(_.shouldFlipToWhite(blackTiles))
    val whiteTilesToFlip = whiteTiles.filter(_.shouldFlipToBlack(blackTiles))

    unchangedBlackTiles ++ whiteTilesToFlip
  }

  object Runner extends Day[List[Tile], Context, Int](2020, 24) {
    override def transformInput(lines: List[String]): List[Tile] = {
      val directionLists = lines.map(parseDirectionList)
      val tiles = directionLists.map(followDirections)
      tilesToFlip(tiles)
    }

    override def partOneContext(): Option[Context] =
      Some(Context(processPartOne))

    override def partTwoContext(): Option[Context] =
      Some(Context(processPartTwo))

    override def process(input: List[Tile], context: Option[Context]): Option[Int] =
      context.map(_.process(input))
  }

  override def run(args: List[String]): IO[ExitCode] = Runner.run(args)
}
