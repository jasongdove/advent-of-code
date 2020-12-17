package adventofcode

case class Point(x: Int, y: Int) {
  val distanceFromOrigin: Int = math.abs(x) + math.abs(y)
}

case class Point4d(x: Int, y: Int, z: Int, w: Int)