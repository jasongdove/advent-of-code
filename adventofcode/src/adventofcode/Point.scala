package adventofcode

case class Point(x: Int, y: Int) {
  val distanceFromOrigin: Int = math.abs(x) + math.abs(y)

  def adjacent: List[Point] = List(
    Point(x, y - 1),
    Point(x - 1, y),
    Point(x + 1, y),
    Point(x, y + 1)
  )
}

case class Point4d(x: Int, y: Int, z: Int, w: Int)
