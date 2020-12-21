package adventofcode.year2020

import adventofcode.utils._
import adventofcode.{Day, Grid, GridLocation}

case class Day20TileEdge(top: String, right: String, bottom: String, left: String)

object Day20TileEdge {
  def from(data: List[String]): Day20TileEdge = {
    Day20TileEdge(
      data.head,
      data.map(_.last).mkString,
      data.last.reverse,
      data.map(_.head).mkString.reverse
    )
  }
}
case class Day20Tile(id: Long, data: List[String]) {
  private val flippedData: List[String] = data.map(_.reverse)

  val edge: Day20TileEdge = Day20TileEdge.from(data)
  val flippedEdge: Day20TileEdge = Day20TileEdge.from(flippedData)

  def debug(): Unit = data.foreach(println)

  lazy val allOrientations: List[Day20Tile] = {
    List(
      this,
      Day20Tile(id, data.rotate90()),
      Day20Tile(id, data.rotate180()),
      Day20Tile(id, data.rotate270()),
      Day20Tile(id, flippedData),
      Day20Tile(id, flippedData.rotate90()),
      Day20Tile(id, flippedData.rotate180()),
      Day20Tile(id, flippedData.rotate270())
    )
  }

  val distinctEdges: List[String] = List(
    edge.top,
    edge.right,
    edge.bottom,
    edge.left,
    flippedEdge.top,
    flippedEdge.right,
    flippedEdge.bottom,
    flippedEdge.left
  )
}

object Day20Tile {
  private val idPattern = "Tile (\\d+):".r

  def parseAll(lines: List[String]): List[Day20Tile] =
    lines.map { line =>
      val split = line.split("\n")
      val idPattern(id) = split.head
      Day20Tile(id.toLong, split.tail.toList)
    }
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
    Day20Tile.parseAll(lines)

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
      case (id, connected) if connected.size == 2 => id
    }.product

  private def processPartTwo(tiles: List[Day20Tile]): Long = {
    val solved = solve(tiles)
    val image = imageFromGrid(solved)
    val allImages = {
      val lines = image.split("\n").toList
      val flipped = lines.map(_.reverse)
      List(
        lines,
        lines.rotate90(),
        lines.rotate180(),
        lines.rotate270(),
        flipped,
        flipped.rotate90(),
        flipped.rotate180(),
        flipped.rotate270()
      )
    }
    val seaMonsterCount = allImages.map(countMonstersInImage).max
    val seaMonsterSize = seaMonster.map(_.pattern.toString.toList.count(c => c == '#')).sum
    val waves = image.count(_ == '#')
    waves - seaMonsterSize * seaMonsterCount.toLong
  }

  private def solve(all: List[Day20Tile]): Grid[Day20Tile] = {
    val tileById = all.map(t => t.id -> t).toMap
    val adj = adjacency(all)

    def topCheck(source: Day20Tile, target: Day20Tile): Boolean = target.edge.bottom == source.edge.top.reverse
    def rightCheck(source: Day20Tile, target: Day20Tile): Boolean = target.edge.left == source.edge.right
    def bottomCheck(source: Day20Tile, target: Day20Tile): Boolean = target.edge.top == source.edge.bottom
    def leftCheck(source: Day20Tile, target: Day20Tile): Boolean = target.edge.right == source.edge.left.reverse

    // pick a corner to start
    val (corner, connectedToCorner) = adj.find { case (_, connected) => connected.size == 2 }.head
    val orientedCorner = tileById(corner).allOrientations.find { tile =>
      val rightMatch = tileById(connectedToCorner.head).allOrientations.exists(o => rightCheck(tile, o))
      val bottomMatch = tileById(connectedToCorner.last).allOrientations.exists(o => bottomCheck(tile, o))
      rightMatch && bottomMatch
    }.head

    val gridSize = math.sqrt(all.length.toDouble).toInt
    var start =
      Grid.fill[Option[Day20Tile]](gridSize, gridSize)(None).updated(GridLocation(0, 0), Some(orientedCorner))

    for (row <- 0 until start.rows) {
      for (col <- 0 until start.columns) {
        if (row != 0 || col != 0) {
          val maybeLeft = if (col > 0) start(row, col - 1) else None
          val maybeTop = if (row > 0) start(row - 1, col) else None
          val hasRight = if (col < start.columns - 1) 1 else 0
          val hasBottom = if (row < start.rows - 1) 1 else 0
          val expectedNeighbors = List(maybeLeft, maybeTop).flatten
          val expectedNeighborCount = expectedNeighbors.length + hasRight + hasBottom

          val tileIds = adj
            .filter { case (_, neighbors) =>
              val correctSize = neighbors.size == expectedNeighborCount
              val correctValues = expectedNeighbors.forall(en => neighbors.contains(en.id))
              correctSize && correctValues
            }
            .map { case (id, _) => id }

          val orientedTile = tileIds.flatMap { id =>
            tileById(id).allOrientations.filter { tile =>
              val top = start
                .get(row - 1, col)
                .flatMap(maybeTile => maybeTile.map(target => topCheck(tile, target)))
                .getOrElse(true)
              val left = start
                .get(row, col - 1)
                .flatMap(maybeTile => maybeTile.map(target => leftCheck(tile, target)))
                .getOrElse(true)
              top && left
            }
          }.head

          start = start.updated(GridLocation(row, col), Some(orientedTile))
        }
      }
    }

    Grid(start.rows, start.columns, start.data.map { case (location, maybeTile) => location -> maybeTile.get })
  }

  private def adjacency(tiles: List[Day20Tile]): Map[Long, Set[Long]] = {
    def connectedTo(tile: Day20Tile): List[Long] =
      tiles.filter(t => tile.distinctEdges.count(e => t.distinctEdges.contains(e)) == 2).map(_.id)

    tiles.map(tile => tile.id -> connectedTo(tile).toSet).toMap
  }

  private def imageFromGrid(grid: Grid[Day20Tile]): String = {
    val sb = new StringBuilder
    for (r <- 0 until grid.rows) {
      for (ri <- 1 until 9) {
        for (c <- 0 until grid.columns) {
          sb.append(grid(r, c).data(ri).slice(1, 9))
        }
        sb.append("\n")
      }
    }
    sb.toString
  }

  private def countMonstersInImage(image: List[String]): Int =
    (0 to image.length - 3).map { line =>
      // manually sliding so we can intersect indexes from all matches
      val indexes1 = (0 to image(line).length - 20)
        .map(i => i -> seaMonster(0).matches(image(line).slice(i, i + 20)))
        .collect { case (index, matches) if matches => index }

      val indexes2 = (0 to image(line + 1).length - 20)
        .map(i => i -> seaMonster(1).matches(image(line + 1).slice(i, i + 20)))
        .collect { case (index, matches) if matches => index }

      val indexes3 = (0 to image(line + 2).length - 20)
        .map(i => i -> seaMonster(2).matches(image(line + 2).slice(i, i + 20)))
        .collect { case (index, matches) if matches => index }

      (indexes1 intersect indexes2 intersect indexes3).size
    }.sum
}
