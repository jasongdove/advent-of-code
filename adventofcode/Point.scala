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

case class Point3d(x: Int, y: Int, z: Int) {
    def distanceFromOrigin(): Int = math.abs(x) + math.abs(y) + math.abs(z)

  def +(that: Point3d): Point3d = Point3d(x + that.x, y + that.y, z + that.z)
  def -(that: Point3d): Point3d = Point3d(x - that.x, y - that.y, z - that.z)
}

case class Point4d(x: Int, y: Int, z: Int, w: Int)
