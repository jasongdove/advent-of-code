package adventofcode.year2020

import adventofcode.{Day, Point4d}

sealed abstract class Day17CubeType

object Day17CubeType {
  case object Inactive extends Day17CubeType
  case object Active extends Day17CubeType
}

case class Day17PocketDimension(is4d: Boolean, elements: Map[Point4d, Day17CubeType]) {
  import Day17PocketDimension._

  lazy val active: Map[Point4d, Day17CubeType] = elements.filter(_._2 == Day17CubeType.Active)

  def debug(): Unit = {
    val activePoints = elements.filter(_._2 == Day17CubeType.Active).keys
    for (w <- activePoints.map(_.w).min to activePoints.map(_.w).max)
      for (z <- activePoints.map(_.z).min to activePoints.map(_.z).max) {
        println(s"z=$z, w=$w")
        for (y <- activePoints.map(_.y).min to activePoints.map(_.y).max) {
          for (x <- activePoints.map(_.x).min to activePoints.map(_.x).max) {
            print(elements(Point4d(x, y, z, w)) match {
              case Day17CubeType.Active   => '#'
              case Day17CubeType.Inactive => '.'
            })
          }
          println()
        }
        println()
      }
    println()
  }

  def neighborsOn(location: Point4d): Int = {
    val checkOffsets = if (is4d) checkOffsets4d else checkOffsets3d
    val checks = checkOffsets.collect { case List(x, y, z, w) =>
      Point4d(location.x + x, location.y + y, location.z + z, location.w + w)
    }.distinct
    checks.count(c => elements.getOrElse(c, Day17CubeType.Inactive) == Day17CubeType.Active)
  }

  def elementsForIteration(): Map[Point4d, Day17CubeType] = {
    val activePoints = active.keys
    val result = for {
      x <- activePoints.map(_.x).min - 1 to activePoints.map(_.x).max + 1
      y <- activePoints.map(_.y).min - 1 to activePoints.map(_.y).max + 1
      z <- activePoints.map(_.z).min - 1 to activePoints.map(_.z).max + 1
      w <- if (is4d) activePoints.map(_.w).min - 1 to activePoints.map(_.w).max + 1 else 0 to 0
    } yield Point4d(x, y, z, w) -> elements.getOrElse(Point4d(x, y, z, w), Day17CubeType.Inactive)
    result.toMap
  }
}

object Day17PocketDimension {
  val checkOffsets3d: List[List[Int]] = List
    .fill(3)(List(-1, 0, 1))
    .flatten
    .combinations(3)
    .flatMap(_.permutations)
    .filterNot(_ == List(0, 0, 0))
    .collect { case List(x, y, z) => List(x, y, z, 0) }
    .toList

  val checkOffsets4d: List[List[Int]] = List
    .fill(4)(List(-1, 0, 1))
    .flatten
    .combinations(4)
    .flatMap(_.permutations)
    .filterNot(_ == List(0, 0, 0, 0))
    .toList
}

case class Day17Context(
  is4d: Boolean
)

object Day17 extends Day[Map[Point4d, Day17CubeType], Day17Context, Int](2020, 17) {

  override def transformInput(lines: List[String]): Map[Point4d, Day17CubeType] = {
    lines.zipWithIndex.flatMap { case (line, y) =>
      line.zipWithIndex.map {
        case ('#', x) => (Point4d(x, y, 0, 0), Day17CubeType.Active)
        case (_, x)   => (Point4d(x, y, 0, 0), Day17CubeType.Inactive)
      }
    }.toMap
  }

  override def partOneContext(): Option[Day17Context] =
    Some(Day17Context(is4d = false))

  override def partTwoContext(): Option[Day17Context] =
    Some(Day17Context(is4d = true))

  override def process(input: Map[Point4d, Day17CubeType], context: Option[Day17Context]): Option[Int] =
    context.flatMap { ctx =>
      @annotation.tailrec
      def iteration(
        acc: Map[Point4d, Day17CubeType],
        start: Day17PocketDimension,
        remaining: List[(Point4d, Day17CubeType)]
      ): Day17PocketDimension =
        remaining match {
          case Nil => Day17PocketDimension(ctx.is4d, acc)
          case (location, cubeType) :: tail =>
            val neighborsOn = start.neighborsOn(location)
            val next = cubeType match {
              case Day17CubeType.Active if neighborsOn != 2 && neighborsOn != 3 => Day17CubeType.Inactive
              case Day17CubeType.Inactive if neighborsOn == 3                   => Day17CubeType.Active
              case t: Day17CubeType                                             => t
            }
            iteration(acc.updated(location, next), start, tail)
        }

      val start = Day17PocketDimension(ctx.is4d, input)
      val list = LazyList.iterate(start) { dimension =>
        val elements = dimension.elementsForIteration()
        iteration(elements, dimension, elements.toList)
      }

      val cycles = 6
      val result = list.take(cycles + 1).last
      //result.debug()
      Some(result.active.size)
    }
}
