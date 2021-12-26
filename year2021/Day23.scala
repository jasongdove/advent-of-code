// using java-opt -Xms64m
// using java-opt -Xmx1g

package adventofcode.year2021

import cats.effect._
import adventofcode._

object Day23 extends IOApp {
  private val amphipods = Map(
    'A' -> 1,
    'B' -> 10,
    'C' -> 100,
    'D' -> 1000
  )

  case class Burrow(
    hall: Hallway,
    a: Room,
    b: Room,
    c: Room,
    d: Room
  ) {
    def room(amphipod: Char) = amphipod match {
      case 'A' => a
      case 'B' => b
      case 'C' => c
      case _   => d
    }

    def updateRoom(amphipod: Char, room: Room) = amphipod match {
      case 'A' => copy(a = room)
      case 'B' => copy(b = room)
      case 'C' => copy(c = room)
      case 'D' => copy(d = room)
    }

    def updateRoom(amphipod: Char, hallLoc: Int, room: Room) = amphipod match {
      case 'A' =>
        copy(hall = hall.copy(hallway = hall.hallway.updated(hallLoc, None)), a = room)
      case 'B' =>
        copy(hall = hall.copy(hallway = hall.hallway.updated(hallLoc, None)), b = room)
      case 'C' =>
        copy(hall = hall.copy(hallway = hall.hallway.updated(hallLoc, None)), c = room)
      case 'D' =>
        copy(hall = hall.copy(hallway = hall.hallway.updated(hallLoc, None)), d = room)
    }

    def isOrganized: Boolean =
      a.isAll('A') && b.isAll('B') && c.isAll('C') && d.isAll('D')

    def debug(energy: Int) = {
      def printSpot(spot: Option[Char]) = spot match {
        case Some(value) => print(value)
        case None        => print(".")
      }

      println()
      println(s"energy: $energy")
      println("#############")

      print("#")
      var i = 0
      while (i < hall.hallway.size) {
        printSpot(hall.hallway(i))
        i += 1
      }
      println("#")

      print("###")
      printSpot(a.top)
      print("#")
      printSpot(b.top)
      print("#")
      printSpot(c.top)
      print("#")
      printSpot(d.top)
      println("###")

      print("###")
      printSpot(a.mid1)
      print("#")
      printSpot(b.mid1)
      print("#")
      printSpot(c.mid1)
      print("#")
      printSpot(d.mid1)
      println("###")

      print("###")
      printSpot(a.mid2)
      print("#")
      printSpot(b.mid2)
      print("#")
      printSpot(c.mid2)
      print("#")
      printSpot(d.mid2)
      println("###")

      print("  #")
      printSpot(a.bottom)
      print("#")
      printSpot(b.bottom)
      print("#")
      printSpot(c.bottom)
      print("#")
      printSpot(d.bottom)
      println("#  ")

      println("  #########  ")
    }
  }

  case class Room(top: Option[Char], mid1: Option[Char], mid2: Option[Char], bottom: Option[Char]) {
    def isAll(amphipod: Char) =
      top == Some(amphipod) &&
        mid1 == Some(amphipod) &&
        mid2 == Some(amphipod) &&
        bottom == Some(amphipod)

    def score(amphipod: Char): Int = {
      var score = 0

      top match {
        case Some(`amphipod`) => score += 1
        case Some(_)          => score -= 1
        case _                => ()
      }
      mid1 match {
        case Some(`amphipod`) => score += 2
        case Some(_)          => score -= 2
        case _                => ()
      }
      mid2 match {
        case Some(`amphipod`) => score += 3
        case Some(_)          => score -= 3
        case _                => ()
      }
      bottom match {
        case Some(`amphipod`) => score += 4
        case Some(_)          => score -= 4
        case _                => ()
      }

      score
    }
  }

  object Room {
    val column = Map(
      'A' -> 2,
      'B' -> 4,
      'C' -> 6,
      'D' -> 8
    )
  }

  case class Hallway(hallway: Array[Option[Char]])

  case class Input(burrow: Burrow)

  case object Input {
    def from(input: List[String]): Input = {
      def parse(char: Char): Option[Char] =
        if (amphipods.contains(char)) Some(char) else None

      val three = input(2)
      val four = input(3)

      val atop = parse(three(3))
      val abottom = parse(four(3))
      val btop = parse(three(5))
      val bbottom = parse(four(5))
      val ctop = parse(three(7))
      val cbottom = parse(four(7))
      val dtop = parse(three(9))
      val dbottom = parse(four(9))

      Input(
        Burrow(
          Hallway(Array.fill(11)(Option.empty[Char])),
          // start with the correct letters in the bottom two for part one
          Room(atop, abottom, Some('A'), Some('A')),
          Room(btop, bbottom, Some('B'), Some('B')),
          Room(ctop, cbottom, Some('C'), Some('C')),
          Room(dtop, dbottom, Some('D'), Some('D'))
        )
      )
    }
  }

  case class Context(transformBurrow: Burrow => Burrow, maxPer: Int)

  object Runner extends Day[Input, Context, Long](2021, 23) {
    override def transformInput(lines: List[String]): Input =
      Input.from(lines)

    override def partOneContext(): Option[Context] =
      Some(Context(identity, 4))

    override def partTwoContext(): Option[Context] =
      Some(
        Context(
          b =>
            b.copy(
              a = b.a.copy(bottom = b.a.mid1, mid1 = Some('D'), mid2 = Some('D')),
              b = b.b.copy(bottom = b.b.mid1, mid1 = Some('C'), mid2 = Some('B')),
              c = b.c.copy(bottom = b.c.mid1, mid1 = Some('B'), mid2 = Some('A')),
              d = b.d.copy(bottom = b.d.mid1, mid1 = Some('A'), mid2 = Some('C'))
            ),
          8
        )
      )

    override def process(input: Input, context: Option[Context]): Option[Long] = context.map { ctx =>
      val moveFromHallBuffer = scala.collection.mutable.ArrayBuffer.empty[BurrowEnergy]
      val moveFromRoomBuffer = scala.collection.mutable.ArrayBuffer.empty[BurrowEnergy]
      val invalidSpots = List(2, 4, 6, 8)

      case class BurrowEnergy(burrow: Burrow, energy: Int)

      var firstBurrow = ctx.transformBurrow(input.burrow)

      def moveFromRoom(
        burrow: Burrow,
        energy: Int,
        amphipod: Char,
        loc: Int,
        start: Int
      ): Vector[BurrowEnergy] = {
        moveFromRoomBuffer.clear()

        var minSpot: Int = loc
        while (minSpot > 0 && burrow.hall.hallway(minSpot - 1).isEmpty) {
          minSpot -= 1
        }

        var l = minSpot
        while (l < loc) {
          if (!invalidSpots.contains(l)) {
            val moves = math.abs(loc - l) + start
            moveFromRoomBuffer.addOne(
              BurrowEnergy(
                burrow.copy(hall = burrow.hall.copy(hallway = burrow.hall.hallway.updated(l, Some(amphipod)))),
                energy + amphipods(amphipod) * moves
              )
            )
          }
          l += 1
        }

        var maxSpot: Int = loc
        while (maxSpot < 10 && burrow.hall.hallway(maxSpot + 1).isEmpty) {
          maxSpot += 1
        }

        var r = loc + 1
        while (r <= maxSpot) {
          if (!invalidSpots.contains(r)) {
            val moves = math.abs(loc - r) + start
            moveFromRoomBuffer.addOne(
              BurrowEnergy(
                burrow.copy(hall = burrow.hall.copy(hallway = burrow.hall.hallway.updated(r, Some(amphipod)))),
                energy + amphipods(amphipod) * moves
              )
            )
          }
          r += 1
        }

        moveFromRoomBuffer.toVector
      }

      def moveFromHall(burrow: Burrow, energy: Int): Vector[BurrowEnergy] = {
        moveFromHallBuffer.clear()

        var l = 0
        while (l <= 10) {
          burrow.hall.hallway(l) match {
            case Some(amphipod) =>
              val room = burrow.room(amphipod)
              val moveDepth = (room.top, room.mid1, room.mid2, room.bottom) match {
                case (None, None, None, None)                                     => Some(4)
                case (None, None, None, Some(`amphipod`))                         => Some(3)
                case (None, None, Some(`amphipod`), Some(`amphipod`))             => Some(2)
                case (None, Some(`amphipod`), Some(`amphipod`), Some(`amphipod`)) => Some(1)
                case _                                                            => None
              }

              moveDepth match {
                case Some(depth) =>
                  val column = Room.column(amphipod)
                  val copy = depth match {
                    case 4 => room.copy(bottom = Some(amphipod))
                    case 3 => room.copy(mid2 = Some(amphipod))
                    case 2 => room.copy(mid1 = Some(amphipod))
                    case _ => room.copy(top = Some(amphipod))
                  }
                  val low = if (l < column) l + 1 else column
                  val hi = if (l > column) l - 1 else column
                  if (
                    burrow.hall.hallway.zipWithIndex.forall {
                      case (Some(_), i) if i >= low && i <= hi => false
                      case _                                   => true
                    }
                  ) {
                    moveFromHallBuffer.addOne(
                      BurrowEnergy(
                        burrow.updateRoom(amphipod, l, copy),
                        energy + ((math.abs(l - column) + depth) * amphipods(amphipod))
                      )
                    )
                  }
                case _ => ()
              }
            case _ => ()
          }

          l += 1
        }

        moveFromHallBuffer.toVector
      }

      // prioritize completion and low energy
      // the faster we get *something* complete,
      // the faster we can discard high energy attempts
      def priorityOrder(b: BurrowEnergy): Int =
        (b.burrow.a.score('A') +
          b.burrow.b.score('B') +
          b.burrow.c.score('C') +
          b.burrow.d.score('D')) * 50_000 - b.energy

      val queue = scala.collection.mutable.PriorityQueue.empty[BurrowEnergy](Ordering.by(priorityOrder))
      var best: Int = Int.MaxValue

      queue.enqueue(BurrowEnergy(firstBurrow, 0))

      while (!queue.isEmpty) {
        val head = queue.dequeue()
        if (head.energy >= best) {
          ()
        } else if (head.burrow.isOrganized) {
          if (head.energy < best) {
            best = head.energy
            // println(s"new best: $best, q size: ${queue.size}")
          }
        } else {
          val rooms = amphipods.keys.flatMap { amphipod =>
            val room = head.burrow.room(amphipod)
            val column = Room.column(amphipod)
            (room.top, room.mid1, room.mid2, room.bottom) match {
              case (Some(`amphipod`), Some(`amphipod`), Some(`amphipod`), Some(`amphipod`)) |
                  (None, Some(`amphipod`), Some(`amphipod`), Some(`amphipod`)) |
                  (None, None, Some(`amphipod`), Some(`amphipod`)) | (None, None, None, Some(`amphipod`)) |
                  (None, None, None, None) =>
                // do nothing, we're done here
                Vector.empty[BurrowEnergy]
              case (None, None, None, Some(amph)) =>
                // bottom needs to move
                moveFromRoom(head.burrow.updateRoom(amphipod, room.copy(bottom = None)), head.energy, amph, column, 4)
              case (None, None, Some(amph), _) =>
                // mid2 needs to move
                moveFromRoom(head.burrow.updateRoom(amphipod, room.copy(mid2 = None)), head.energy, amph, column, 3)
              case (None, Some(amph), _, _) =>
                // mid1 needs to move
                moveFromRoom(head.burrow.updateRoom(amphipod, room.copy(mid1 = None)), head.energy, amph, column, 2)
              case (Some(amph), _, _, _) =>
                // top needs to move
                moveFromRoom(head.burrow.updateRoom(amphipod, room.copy(top = None)), head.energy, amph, column, 1)
            }
          }

          val hall = moveFromHall(head.burrow, head.energy)

          val nextQ = (hall ++ rooms)
            .filter(_.energy < best)

          queue.addAll(nextQ)
        }
      }

      best.toLong
    }
  }

  override def run(args: List[String]): IO[ExitCode] = Runner.run(args)
}
