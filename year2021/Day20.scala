// using java-opt -Xms64m
// using java-opt -Xmx1g

package adventofcode.year2021

import cats.effect._
import adventofcode._
import adventofcode.utils._

object Day20 extends IOApp {

  case class Input(algorithm: String, image: Grid[Byte])

  case object Input {
    def from(input: List[String]): Input = {
      val algorithm = input.head
      val image = input.drop(1).filterNot(_.isEmpty())
      val rows = image.length
      val cols = image.head.size
      val data = for {
        (line, row) <- image.zipWithIndex
        (value, col) <- line.zipWithIndex
      } yield GridLocation(row, col) -> (if (value.toString == "#") 1.toByte else 0.toByte)

      Input(input.head, Grid(rows, cols, data.toMap))
    }
  }

  case class Context(steps: Int)

  object Runner extends Day[Input, Context, Long](2021, 20) {
    override def transformInput(lines: List[String]): Input =
      Input.from(lines)

    override def partOneContext(): Option[Context] =
      Some(Context(2))

    override def partTwoContext(): Option[Context] =
      Some(Context(50))

    override def process(input: Input, context: Option[Context]): Option[Long] =
      context.map { ctx =>
        var map = scala.collection.mutable.HashMap.empty[Point, Byte]
        val map2 = scala.collection.mutable.HashMap.empty[Point, Byte]

        for {
          r <- 0 until input.image.rows
          c <- 0 until input.image.columns
        } {
          map.update(Point(c, r), input.image(r, c))
        }

        for (i <- 1 to ctx.steps) {
          val oob = if (i % 2 == 1) 0.toByte else 1.toByte

          map2.clear()

          val minKey = map.filter(kv => kv._2 == 1.toByte).map(kv => math.min(kv._1.x, kv._1.y)).min - 1
          val maxKey = map.filter(kv => kv._2 == 1.toByte).map(kv => math.max(kv._1.x, kv._1.y)).max + 1

          val keys = for {
            r <- minKey to maxKey
            c <- minKey to maxKey
          } yield Point(c, r)

          for (k <- keys)
            if (!map.contains(k))
              map.addOne(Point(k.x, k.y), oob)

          for (k <- keys) {
            val result =
              if (map.contains(k))
                if (input.algorithm(neighbors(map, k, oob)) == '#') 1.toByte else 0.toByte
              else oob
            map2.addOne(k, result)
          }

          map = map2.clone()
        }

        map.values.count(_ == 1.toByte).toLong
      }
  }

  private def neighbors(map: scala.collection.mutable.HashMap[Point, Byte], point: Point, oob: Byte): Int = {
    val string = List(
      Point(point.x - 1, point.y - 1),
      Point(point.x, point.y - 1),
      Point(point.x + 1, point.y - 1),
      Point(point.x - 1, point.y),
      Point(point.x, point.y),
      Point(point.x + 1, point.y),
      Point(point.x - 1, point.y + 1),
      Point(point.x, point.y + 1),
      Point(point.x + 1, point.y + 1)
    ).map(loc => map.get(loc).getOrElse(oob))
      .map {
        case 0 => '0'
        case 1 => '1'
      }
      .mkString

    string.parseBinaryToInt
  }

  override def run(args: List[String]): IO[ExitCode] = Runner.run(args)
}
