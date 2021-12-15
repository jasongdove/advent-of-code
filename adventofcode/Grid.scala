package adventofcode

case class GridValue[A](location: GridLocation, value: A)
case class GridLocation(row: Int, col: Int)
case class Grid[A](rows: Int, columns: Int, data: Map[GridLocation, A]) {
  def updated(location: GridLocation, value: A): Grid[A] =
    Grid(rows, columns, data.updated(location, value))

  def apply(row: Int, column: Int): A = data(GridLocation(row, column))

  def get(row: Int, column: Int): Option[A] = data.get(GridLocation(row, column))

  def neighbors(row: Int, column: Int): Iterable[GridValue[A]] =
    List(
      GridLocation(row - 1, column),
      GridLocation(row + 1, column),
      GridLocation(row, column - 1),
      GridLocation(row, column + 1)
    ).filter(loc => loc.row >= 0 && loc.row < rows && loc.col >= 0 && loc.col < columns)
      .map(loc => GridValue(loc, data(loc)))

  def diagonalNeighbors(row: Int, column: Int): Iterable[GridValue[A]] =
    List(
      GridLocation(row - 1, column - 1),
      GridLocation(row - 1, column),
      GridLocation(row - 1, column + 1),
      GridLocation(row, column - 1),
      GridLocation(row, column + 1),
      GridLocation(row + 1, column - 1),
      GridLocation(row + 1, column),
      GridLocation(row + 1, column + 1)
    ).filter(loc => loc.row >= 0 && loc.row < rows && loc.col >= 0 && loc.col < columns)
      .map(loc => GridValue(loc, data(loc)))

  def allLocations(): Iterable[GridLocation] = data.map(_._1)

  def format(format: GridLocation => String): String = {
    val result = new scala.collection.mutable.StringBuilder()
    for (r <- 0 until rows) {
      for (c <- 0 until columns) {
        result.append(format(GridLocation(r, c)))
      }
      result.append("\n")
    }
    result.append("\n")
    result.toString
  }
}

object Grid {
  def fill[A](rows: Int, columns: Int)(e: A): Grid[A] = {
    val data = for {
      r <- 0 until rows
      c <- 0 until columns
    } yield GridLocation(r, c)
    Grid(rows, columns, data.map(_ -> e).toMap)
  }

  implicit def ordering: Ordering[GridLocation] = new Ordering[GridLocation] {
    override def compare(x: GridLocation, y: GridLocation): Int = {
      (x.row compare y.row) match {
        case 0 => x.col compare y.col
        case c => c
      }
    }
  }
}
