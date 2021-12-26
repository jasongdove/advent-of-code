// using java-opt -Xms64m
// using java-opt -Xmx2g

package adventofcode.year2021

import cats.effect._
import adventofcode._
import cats.instances.int

object Day19 extends IOApp {
  case class PointsInCommon(offset: Point3d, points: List[(Point3d, Point3d)], transformed: Scanner)

  case class RelativeScanner(
    number: Int,
    relativeTo: Point3d,
    beacons: List[Point3d],
    orientation: Point3d => Point3d
  ) {
    override def toString: String = {
      var s = s"--- scanner $number ---\n"
      beacons.foreach(p => s += s"${p.x},${p.y},${p.z}\n")
      s
    }

    def unrelative: Scanner = {
      Scanner(number, beacons.map(p => Point3d(p.x + relativeTo.x, p.y + relativeTo.y, p.z + relativeTo.z)), identity)
    }
  }

  case class Scanner(number: Int, beacons: List[Point3d], orientation: Point3d => Point3d) {
    val orientationList: Seq[(Point3d => Point3d, Point3d => Point3d)] =
      Seq(
        (identity, identity),
        (p => Point3d(-p.y, p.x, p.z), p => Point3d(p.y, -p.x, p.z)),
        (p => Point3d(p.x, -p.y, p.z), p => Point3d(p.x, -p.y, p.z)),
        (p => Point3d(p.y, -p.x, p.z), p => Point3d(-p.y, p.x, p.z)),
        (p => Point3d(-p.x, p.y, -p.z), p => Point3d(-p.x, p.y, -p.z)),
        (p => Point3d(-p.y, -p.x, -p.z), p => Point3d(-p.y, -p.x, -p.z)),
        (p => Point3d(-p.x, -p.y, -p.z), p => Point3d(-p.x, -p.y, -p.z)),
        (p => Point3d(p.y, p.x, -p.z), p => Point3d(p.y, p.x, -p.z)),
        (p => Point3d(p.z, p.y, -p.x), p => Point3d(-p.z, p.y, p.x)),
        (p => Point3d(p.z, p.x, -p.y), p => Point3d(p.y, -p.z, p.x)),
        (p => Point3d(p.z, -p.y, p.x), p => Point3d(p.z, -p.y, p.x)),
        (p => Point3d(p.z, -p.x, p.y), p => Point3d(-p.y, p.z, p.x)),
        (p => Point3d(-p.z, p.y, p.x), p => Point3d(p.z, p.y, -p.x)),
        (p => Point3d(-p.z, -p.x, -p.y), p => Point3d(-p.y, -p.z, -p.x)),
        (p => Point3d(-p.z, -p.y, -p.x), p => Point3d(-p.z, -p.y, -p.x)),
        (p => Point3d(-p.z, p.x, p.y), p => Point3d(p.y, p.z, -p.x)),
        (p => Point3d(p.x, p.z, -p.y), p => Point3d(p.x, -p.z, p.y)),
        (p => Point3d(-p.y, p.z, -p.x), p => Point3d(-p.z, -p.x, p.y)),
        (p => Point3d(-p.x, p.z, p.y), p => Point3d(-p.x, p.z, p.y)),
        (p => Point3d(p.y, p.z, p.x), p => Point3d(p.z, p.x, p.y)),
        (p => Point3d(p.x, -p.z, p.y), p => Point3d(p.x, p.z, -p.y)),
        (p => Point3d(p.y, -p.z, -p.x), p => Point3d(-p.z, p.x, -p.y)),
        (p => Point3d(-p.x, -p.z, -p.y), p => Point3d(-p.x, -p.z, -p.y)),
        (p => Point3d(-p.y, -p.z, p.x), p => Point3d(p.z, -p.x, -p.y))
      )

    def orientations(): LazyList[Scanner] =
      LazyList
        .from(orientationList)
        .map { o => copy(beacons = beacons.map(o._1), orientation = o._2) }

    def relativeTo(origin: Point3d): RelativeScanner =
      RelativeScanner(
        number,
        origin,
        beacons.map(p => Point3d(p.x - origin.x, p.y - origin.y, p.z - origin.z)),
        orientation
      )

    def relativeOrientations: LazyList[(Scanner, List[RelativeScanner])] =
      orientations.map { o =>
        val relatives = o.beacons.map(p => RelativeScanner(o.number, p, o.relativeTo(p).beacons, o.orientation)).toList
        o -> relatives
      }

    def pointsInCommon(other: Scanner): PointsInCommon = {
      val relativeOtherOrientations = other.relativeOrientations

      @annotation.tailrec
      def loop(q: Vector[RelativeScanner], best: PointsInCommon): PointsInCommon = {
        q match {
          case head +: tail =>
            val nextBest = relativeOtherOrientations
              .flatMap { case (orientation, ls) =>
                ls.map { s =>
                  def orientToThis(points: List[Point3d]): List[(Point3d, Point3d, Point3d)] = points.map { p =>
                    val headBeacon =
                      head.orientation(head.unrelative.beacons.apply(head.beacons.indexOf(p)))
                    val otherBeacon = head.orientation(orientation.beacons.apply(s.beacons.indexOf(p)))
                    val otherOffset =
                      Point3d(
                        headBeacon.x - otherBeacon.x,
                        headBeacon.y - otherBeacon.y,
                        headBeacon.z - otherBeacon.z
                      )

                    (
                      otherOffset,
                      headBeacon,
                      s.orientation(s.unrelative.beacons.apply(s.beacons.indexOf(p)))
                    )
                  }

                  val intersected = orientToThis(s.beacons.intersect(head.beacons))
                  val diff = intersected.headOption.map { h => h._1 }

                  val all = diff
                    .map(d => s.unrelative.beacons.map(b => head.orientation(b) + d))
                    .getOrElse(s.beacons)

                  PointsInCommon(
                    intersected.head._1,
                    intersected.map { kv => kv._2 -> kv._3 },
                    Scanner(number, all, identity)
                  )
                }.find(_.points.size >= 12)
              }
              .find(_.points.size >= 12)

            if (nextBest.isDefined) nextBest.get
            else loop(tail, best)

          case _ => best
        }
      }

      loop(
        relativeOrientations.flatMap(_._2).toVector,
        PointsInCommon(Point3d(0, 0, 0), List.empty, Scanner(0, List.empty, identity))
      )
    }

    override def toString: String = {
      var s = s"--- scanner $number ---\n"
      beacons.foreach(p => s += s"${p.x},${p.y},${p.z}\n")
      s
    }
  }

  case class Input(scanners: List[Scanner])

  case object Input {
    val scannerPattern = "--- scanner (\\d+) ---".r
    val beaconPattern = "(-?\\d+),(-?\\d+),(-?\\d+)".r

    def from(input: List[String]): Input = {
      var scanner = Scanner(-1, List.empty, identity)
      var result = scala.collection.mutable.ArrayBuffer.empty[Scanner]
      for (line <- input) {
        line match {
          case scannerPattern(number) =>
            if (scanner.number >= 0) {
              result.addOne(scanner)
            }
            scanner = Scanner(number.toInt, List.empty, identity)
          case beaconPattern(x, y, z) =>
            scanner = scanner.copy(beacons = scanner.beacons :+ Point3d(x.toInt, y.toInt, z.toInt))
          case _ =>
        }
      }

      if (scanner.number >= 0) {
        result.addOne(scanner)
      }

      Input(result.toList)
    }
  }

  case class Context(aggregate: (Scanner, Map[Int, Point3d]) => Long)

  object Runner extends Day[Input, Context, Long](2021, 19) {
    override def transformInput(lines: List[String]): Input =
      Input.from(lines)

    override def partOneContext(): Option[Context] =
      Some(Context((s: Scanner, _) => s.beacons.size.toLong))

    override def partTwoContext(): Option[Context] =
      Some(
        Context((_, m: Map[Int, Point3d]) =>
          m.map(_._2)
            .toSet
            .subsets(2)
            .map(s => {
              val head = s.head
              val last = s.last
              Math.abs(head.x - last.x) + Math.abs(head.y - last.y) + Math.abs(head.z - last.z)
            })
            .max
        )
      )

    override def process(input: Input, context: Option[Context]): Option[Long] =
      context.map { ctx =>
        val offsets = scala.collection.mutable.Map.empty[Int, Point3d]

        @annotation.tailrec
        def loop(q: Vector[Scanner], acc: Scanner): Scanner = {
          q match {
            case head +: tail =>
              val common = acc.pointsInCommon(head)
              if (common.points.size == 0) {
                loop(tail :+ head, acc)
              } else {
                offsets.update(head.number, common.offset)
                val nextAcc = acc.copy(beacons = (acc.beacons ++ common.transformed.beacons).distinct)
                loop(tail, nextAcc)
              }
            case _ => acc
          }
        }

        // "optimization"
        val all =
          if (input.scanners.length > 5)
            List(0, 17, 20, 29, 24, 10, 23, 11, 30, 7, 21, 25, 6, 19, 27, 22, 5, 33, 14, 16, 32, 8, 31, 4, 12, 28, 9, 1,
              18, 2, 3, 13, 26, 15).map { n =>
              input.scanners.find(_.number == n).head
            }.toVector
          else input.scanners.toVector

        val zero = input.scanners.find(_.number == 0).head

        val result = loop(all, zero)

        ctx.aggregate(result, offsets.toMap)
      }
  }

  override def run(args: List[String]): IO[ExitCode] = Runner.run(args)
}
