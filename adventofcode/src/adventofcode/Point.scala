package adventofcode

case class Point(x: Int, y: Int) {
    val distanceFromOrigin = math.abs(x) + math.abs(y)
}