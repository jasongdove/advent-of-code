package adventofcode

case class GridLocation(row: Int, col: Int)
case class Grid[A](rows: Int, columns: Int, data: Map[GridLocation, A]) {
  def updated(location: GridLocation, value: A): Grid[A] =
    Grid(rows, columns, data.updated(location, value))
  def apply(row: Int, column: Int): A = data(GridLocation(row, column))
  def get(row: Int, column: Int): Option[A] = data.get(GridLocation(row, column))
}

object Grid {
  def fill[A](rows: Int, columns: Int)(e: A): Grid[A] = {
    val data = for {
      r <- 0 until rows
      c <- 0 until columns
    } yield GridLocation(r, c)
    Grid(rows, columns, data.map(_ -> e).toMap)
  }
}
