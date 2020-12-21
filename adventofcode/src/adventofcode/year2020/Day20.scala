package adventofcode.year2020

import adventofcode.utils._
import adventofcode.{Day, Grid, GridLocation}

case class Day20TileEdges(id: Long, a: String, b: String, c: String, d: String, completeTile: List[String])

case class Day20Tile(id: Long, edgeOptions: List[Day20TileEdges]) {
  lazy val allEdges: List[Day20TileEdges] = {
    val fa = edgeOptions.head.a
    val fb = edgeOptions.head.b
    val fc = edgeOptions.head.c
    val fd = edgeOptions.head.d

    val ra = edgeOptions(1).a
    val rb = edgeOptions(1).b
    val rc = edgeOptions(1).c
    val rd = edgeOptions(1).d

    val forwardTile = edgeOptions.head.completeTile.map(_.toList)
    val reverseTile = edgeOptions(1).completeTile.map(_.toList)

    List(
      Day20TileEdges(id, fa, fb, fc, fd, forwardTile.map(_.mkString)),
      Day20TileEdges(id, fd, fa, fb, fc, forwardTile.rotate90().map(_.mkString)),
      Day20TileEdges(id, fc, fd, fa, fb, forwardTile.rotate180().map(_.mkString)),
      Day20TileEdges(id, fb, fc, fd, fa, forwardTile.rotate270().map(_.mkString)),
      Day20TileEdges(id, ra, rb, rc, rd, reverseTile.map(_.mkString)),
      Day20TileEdges(id, rd, ra, rb, rc, reverseTile.rotate90().map(_.mkString)),
      Day20TileEdges(id, rc, rd, ra, rb, reverseTile.rotate180().map(_.mkString)),
      Day20TileEdges(id, rb, rc, rd, ra, reverseTile.rotate270().map(_.mkString))
    )
  }
  val distinctEdges: List[String] = edgeOptions.flatMap(e => List(e.a, e.b, e.c, e.d)).distinct.sorted
}

object Day20Data {
  private val idPattern = "Tile (\\d+):".r

  def parseAll(lines: List[String]): List[Day20Tile] = {
    // find all edges (with reverses) and normalize tiles to use simple numbers
    val tileLines = lines.map { l =>
      val split = l.split("\n")
      val idPattern(id) = split.head
      val patternLines = split.tail.toList
      val a = patternLines.head
      val b = patternLines.map(l => l.last).mkString
      val c = patternLines.last.reverse
      val d = patternLines.map(l => l.head).mkString.reverse
      TileLines(id.toLong, a, b, c, d, patternLines)
    }

    tileLines.map { line =>
      val edges1 = Day20TileEdges(
        line.id,
        line.a,
        line.b,
        line.c,
        line.d,
        line.complete
      )
      val edges2 = Day20TileEdges(
        line.id,
        line.a.reverse,
        line.d.reverse,
        line.c.reverse,
        line.b.reverse,
        line.complete.map(_.reverse)
      )
      Day20Tile(line.id, List(edges1, edges2))
    }
  }

  case class TileLines(id: Long, a: String, b: String, c: String, d: String, complete: List[String])
}

case class Day20Context(process: List[Day20Tile] => Long)

object Day20 extends Day[List[Day20Tile], Day20Context, Long](2020, 20) {
  // format: off
  private val seaMonster =
    ("                  # \n" +
      "#    ##    ##    ###\n" +
      " #  #  #  #  #  #   \n")
      .replace(" ", ".")
      .split("\n")
      .map(line => s"$line".r)
  // format: on

  override def transformInput(lines: List[String]): List[Day20Tile] =
    Day20Data.parseAll(lines)

  override def splitOn(): String = "\n\n"

  override def partOneContext(): Option[Day20Context] =
    Some(Day20Context(processPartOne))

  override def partTwoContext(): Option[Day20Context] =
    Some(Day20Context(processPartTwo))

  override def process(input: List[Day20Tile], context: Option[Day20Context]): Option[Long] =
    context.map(_.process(input))

  private def processPartOne(tiles: List[Day20Tile]): Long =
    // corners are only connected to two other tiles
    adjacency(tiles).collect {
      case (tile, connected) if connected.size == 2 => tile.id
    }.product

  private def processPartTwo(tiles: List[Day20Tile]): Long = {
    val solved = solve(tiles)
    val oriented = for {
      r <- 0 until solved.rows
      c <- 0 until solved.columns
    } yield {
      val tile = solved(r, c)
      val topCondition: String => Boolean = s => r == 0 || solved(r - 1, c).distinctEdges.contains(s)
      val leftCondition: String => Boolean = s => c == 0 || solved(r, c - 1).distinctEdges.contains(s)
      val rightCondition: String => Boolean = s => c == solved.columns - 1 || solved(r, c + 1).distinctEdges.contains(s)
      val bottomCondition: String => Boolean = s => r == solved.rows - 1 || solved(r + 1, c).distinctEdges.contains(s)
      val result = tile.allEdges.find { e =>
        topCondition(e.a) && leftCondition(e.d) && rightCondition(e.b) && bottomCondition(e.c)
      }
      GridLocation(r, c) -> result
    }
    val finalGrid = Grid(solved.rows, solved.columns, oriented.map(kv => kv._1 -> kv._2.get).toMap)
    val arranged = {
      val sb = new StringBuilder
      for (r <- 0 until finalGrid.rows) {
        for (ri <- 1 until 9) {
          for (c <- 0 until finalGrid.columns) {
            sb.append(finalGrid(r, c).completeTile(ri).slice(1, 9))
            //sb.append(" ")
          }
          sb.append("\n")
        }
        //sb.append("\n")
      }
      sb.toString
    }
    // println(arranged)

    def countMonsters(lines: List[String]): Int = {
      (0 to lines.length - 3).map { line =>
        val indexes1 = (0 to lines(line).length - 20)
          .map(i => i -> seaMonster(0).matches(lines(line + 1).slice(i, i + 20)))
          .filter(_._2)
          .map(_._1)
        val indexes2 = (0 to lines(line + 1).length - 20)
          .map(i => i -> seaMonster(1).matches(lines(line + 1).slice(i, i + 20)))
          .filter(_._2)
          .map(_._1)
        val indexes3 = (0 to lines(line + 2).length - 20)
          .map(i => i -> seaMonster(2).matches(lines(line + 2).slice(i, i + 20)))
          .filter(_._2)
          .map(_._1)
        (indexes1 intersect indexes2 intersect indexes3).size
      }.sum
    }

    val lines = arranged.split("\n").map(_.toList).toList
    val reversed = lines.map(_.reverse)
    val allArrangements = List(
      lines.map(_.mkString),
      lines.rotate90().map(_.mkString),
      lines.rotate180().map(_.mkString),
      lines.rotate270().map(_.mkString),
      reversed.map(_.mkString),
      reversed.rotate90().map(_.mkString),
      reversed.rotate180().map(_.mkString),
      reversed.rotate270().map(_.mkString)
    )

    val seaMonsterCount = allArrangements.map(countMonsters).max
    val seaMonsterSize = seaMonster.map(_.pattern.toString.toList.count(c => c == '#')).sum
    val waves = arranged.count(_ == '#')
    waves - seaMonsterSize * seaMonsterCount.toLong
  }

  private def solve(all: List[Day20Tile]): Grid[Day20Tile] = {
    var adj = adjacency(all)

    def removeConnection(one: Day20Tile, two: Day20Tile): Unit = {
      adj = adj.updated(one, adj(one) - two)
      adj = adj.updated(two, adj(two) - one)
    }

    // pick a corner to start
    val corner = adj.collect { case (tile, connected) if connected.size == 2 => tile }.head
    val gridSize = math.sqrt(all.length.toDouble).toInt
    var start =
      Grid.fill[Option[Day20Tile]](gridSize, gridSize)(None).updated(GridLocation(0, 0), Some(corner))

    for (row <- 0 until start.rows) {
      for (col <- 0 until start.columns) {
        val maybeLeft = if (col > 0) start(row, col - 1) else None
        val maybeTop = if (row > 0) start(row - 1, col) else None
        val hasRight = if (col < start.columns - 1) 1 else 0
        val hasBottom = if (row < start.rows - 1) 1 else 0
        val expectedNeighbors = List(maybeLeft, maybeTop).flatten
        val expectedNeighborCount = expectedNeighbors.length + hasRight + hasBottom

        val maybeTile = adj
          .find { case (_, neighbors) =>
            val correctSize = neighbors.size == expectedNeighborCount
            val correctValues = expectedNeighbors.forall(en => neighbors.contains(en))
            correctSize && correctValues
          }
          .map { case (tile, _) => tile }

        maybeTile.foreach { tile =>
          start = start.updated(GridLocation(row, col), Some(tile))
          maybeLeft.foreach(left => removeConnection(tile, left))
          maybeTop.foreach(top => removeConnection(tile, top))
        }
      }
    }

    Grid(start.rows, start.columns, start.data.map { case (location, maybeTile) => location -> maybeTile.get })
  }

  private def adjacency(tiles: List[Day20Tile]): Map[Day20Tile, Set[Day20Tile]] = {
    def connectedTo(tile: Day20Tile, tiles: List[Day20Tile]): List[Day20Tile] =
      tiles.filter(t => tile.distinctEdges.count(e => t.distinctEdges.contains(e)) == 2)

    tiles.map(tile => tile -> connectedTo(tile, tiles).toSet).toMap
  }
}
