package adventofcode.year2022

import adventofcode.Day
import cats.effect._

object Day22 extends IOApp {
  sealed trait BoardElement

  case object BoardElement {
    case object OpenTile extends BoardElement
    case object SolidWall extends BoardElement
    case object WrapAround extends BoardElement
  }

  case class BoardLocation(row: Int, col: Int) {
    def add(other: BoardLocation, minRows: Int, maxRows: Int, minCols: Int, maxCols: Int) = {
      val nextRow = (row + other.row) match {
        case r if r > maxRows => minRows
        case r if r < minRows => maxRows
        case r                => r
      }

      val nextCol = (col + other.col) match {
        case c if c > maxCols => minCols
        case c if c < minCols => maxCols
        case c                => c
      }

      BoardLocation(nextRow, nextCol)
    }
  }

  case class FoldLocation(location: BoardLocation, facing: Facing)

  case class Board(elements: Map[BoardLocation, BoardElement], faceSize: Int)

  sealed trait Facing

  case object Facing {
    case object Left extends Facing
    case object Right extends Facing
    case object Up extends Facing
    case object Down extends Facing
  }

  sealed trait TurnDirection

  case object TurnDirection {
    case object Clockwise extends TurnDirection
    case object CounterClockwise extends TurnDirection
  }

  sealed trait PathStep

  case object PathStep {
    case class Move(distance: Int) extends PathStep
    case class Turn(direction: TurnDirection) extends PathStep
  }

  case class Input(board: Board, path: List[PathStep])

  case object Input {
    def from(lines: List[String]): Input = {
      val top = lines.dropRight(3)
      val maxLength = lines.map(_.size).max
      val boardElements = top.zipWithIndex.flatMap { case (line, row) =>
        line.padTo(maxLength, ' ').zipWithIndex.map {
          case ('#', col) => (BoardLocation(row + 1, col + 1), BoardElement.SolidWall)
          case ('.', col) => (BoardLocation(row + 1, col + 1), BoardElement.OpenTile)
          case (_, col)   => (BoardLocation(row + 1, col + 1), BoardElement.WrapAround)
        }
      }.toMap

      val path = lines
        .takeRight(2)
        .head
        .replace("R", ",R,")
        .replace("L", ",L,")
        .split(',')
        .map(step =>
          step match {
            case "R" => PathStep.Turn(TurnDirection.Clockwise)
            case "L" => PathStep.Turn(TurnDirection.CounterClockwise)
            case s   => PathStep.Move(s.toInt)
          }
        )
        .toList

      val faceSize = lines.last.toInt

      Input(Board(boardElements, faceSize), path)
    }
  }

  def walk(
    board: Board,
    path: List[PathStep],
    foldLocations: Map[FoldLocation, FoldLocation]
  ): (BoardLocation, Facing) = {
    val minRows = (board.elements.map(_._1.row) ++ foldLocations.keySet.map(_.location.row)).min
    val maxRows = (board.elements.map(_._1.row) ++ foldLocations.keySet.map(_.location.row)).max
    val minCols = (board.elements.map(_._1.col) ++ foldLocations.keySet.map(_.location.col)).min
    val maxCols = (board.elements.map(_._1.col) ++ foldLocations.keySet.map(_.location.col)).max

    val startRow = 1
    val startCol = board.elements
      .filter {
        case (location, BoardElement.OpenTile) => location.row == startRow
        case _                                 => false
      }
      .map(_._1.col)
      .min

    var currentLocation = BoardLocation(startRow, startCol)
    var currentFacing: Facing = Facing.Right

    // var lastFacing = scala.collection.mutable.Map.from(
    //   board.elements.view.mapValues(element =>
    //     element match {
    //       case BoardElement.OpenTile  => '.'
    //       case BoardElement.SolidWall => '#'
    //       case _                      => ' '
    //     }
    //   )
    // )

    for (pathStep <- path) {
      pathStep match {
        case PathStep.Move(distance) =>
          for (_ <- 1 to distance) {
            var nextLocation = currentLocation
            var nextElement = board.elements(nextLocation)
            var nextFacing = currentFacing
            do {
              val delta = nextFacing match {
                case Facing.Right => BoardLocation(0, 1)
                case Facing.Down  => BoardLocation(1, 0)
                case Facing.Left  => BoardLocation(0, -1)
                case Facing.Up    => BoardLocation(-1, 0)
              }

              nextLocation = nextLocation.add(delta, minRows, maxRows, minCols, maxCols)
              val foldKey = FoldLocation(nextLocation, nextFacing)
              if (foldLocations.contains(foldKey)) {
                val FoldLocation(foldTarget, foldFacing) = foldLocations(foldKey)
                nextLocation = foldTarget
                nextFacing = foldFacing
              }

              nextElement = board.elements(nextLocation)
            } while (nextElement == BoardElement.WrapAround)
            if (nextElement == BoardElement.OpenTile) {
              // val debugChar = nextFacing match {
              //   case Facing.Down  => 'v'
              //   case Facing.Up    => '^'
              //   case Facing.Right => '>'
              //   case Facing.Left  => '<'
              // }
              // lastFacing.update(nextLocation, debugChar)
              currentLocation = nextLocation
              currentFacing = nextFacing
            }
          }
        case PathStep.Turn(TurnDirection.Clockwise) =>
          currentFacing = currentFacing match {
            case Facing.Right => Facing.Down
            case Facing.Down  => Facing.Left
            case Facing.Left  => Facing.Up
            case Facing.Up    => Facing.Right
          }
        // val debugChar = currentFacing match {
        //   case Facing.Down  => 'v'
        //   case Facing.Up    => '^'
        //   case Facing.Right => '>'
        //   case Facing.Left  => '<'
        // }
        // lastFacing.update(currentLocation, debugChar)
        case PathStep.Turn(TurnDirection.CounterClockwise) =>
          currentFacing = currentFacing match {
            case Facing.Right => Facing.Up
            case Facing.Down  => Facing.Right
            case Facing.Left  => Facing.Down
            case Facing.Up    => Facing.Left
          }
        // val debugChar = currentFacing match {
        //   case Facing.Down  => 'v'
        //   case Facing.Up    => '^'
        //   case Facing.Right => '>'
        //   case Facing.Left  => '<'
        // }
        // lastFacing.update(currentLocation, debugChar)
      }

      // for (r <- minRows to maxRows) {
      //   for (c <- minCols to maxCols) {
      //     val location = BoardLocation(r, c)
      //     if (lastFacing.contains(location)) {
      //       print(lastFacing(location))
      //     } else if (board.elements.contains(location)) {
      //       board.elements(location) match {
      //         case BoardElement.OpenTile  => '.'
      //         case BoardElement.SolidWall => '#'
      //         case _                      => ' '
      //       }
      //     } else {
      //       print(' ')
      //     }
      //   }
      //   println()
      // }
    }

    (currentLocation, currentFacing)
  }

  case class Context(getFacings: Board => Map[FoldLocation, FoldLocation])

  object Runner extends Day[Input, Context, Int](2022, 22) {
    override def transformInput(lines: List[String]): Input =
      Input.from(lines)

    override def partOneContext(): Option[Context] =
      Some(Context(_ => Map.empty))

    override def partTwoContext(): Option[Context] =
      Some(Context(board => {
        import BoardElement._
        import Facing._

        val facings = scala.collection.mutable.Map.empty[FoldLocation, FoldLocation]

        if (board.faceSize == 4) {
          // a
          for (c <- 9 to 12) {
            facings.addOne(FoldLocation(BoardLocation(0, c), Up), FoldLocation(BoardLocation(5, 13 - c), Down))
            facings.addOne(FoldLocation(BoardLocation(4, 13 - c), Up), FoldLocation(BoardLocation(1, c), Down))
          }
          // b
          for (c <- 5 to 8) {
            facings.addOne(FoldLocation(BoardLocation(4, c), Up), FoldLocation(BoardLocation(c - 4, 9), Right))
            facings.addOne(FoldLocation(BoardLocation(c - 4, 8), Left), FoldLocation(BoardLocation(5, c), Down))
          }
          // c
          for (r <- 5 to 8) {
            facings.addOne(FoldLocation(BoardLocation(r, 13), Right), FoldLocation(BoardLocation(9, 21 - r), Down))
            facings.addOne(FoldLocation(BoardLocation(8, 21 - r), Up), FoldLocation(BoardLocation(r, 12), Left))
          }
          // d
          for (r <- 1 to 4) {
            facings.addOne(FoldLocation(BoardLocation(r, 13), Right), FoldLocation(BoardLocation(13 - r, 16), Left))
            facings.addOne(FoldLocation(BoardLocation(13 - r, 17), Right), FoldLocation(BoardLocation(r, 12), Left))
          }
          // e
          for (c <- 5 to 8) {
            facings.addOne(FoldLocation(BoardLocation(9, c), Down), FoldLocation(BoardLocation(17 - c, 9), Right))
            facings.addOne(FoldLocation(BoardLocation(17 - c, 8), Left), FoldLocation(BoardLocation(8, c), Up))
          }
          // f
          for (c <- 1 to 4) {
            facings.addOne(FoldLocation(BoardLocation(9, c), Down), FoldLocation(BoardLocation(12, 13 - c), Up))
            facings.addOne(FoldLocation(BoardLocation(13, 13 - c), Down), FoldLocation(BoardLocation(8, c), Up))
          }
          // g
          for (r <- 5 to 8) {
            facings.addOne(FoldLocation(BoardLocation(r, 0), Left), FoldLocation(BoardLocation(12, 21 - r), Up))
            facings.addOne(FoldLocation(BoardLocation(13, 21 - r), Down), FoldLocation(BoardLocation(r, 1), Right))
          }
        } else if (board.faceSize == 50) {
          // B top => D left
          for (c <- 1 to 50) {
            facings.addOne(FoldLocation(BoardLocation(100, c), Up), FoldLocation(BoardLocation(50 + c, 51), Right))
            facings.addOne(FoldLocation(BoardLocation(50 + c, 50), Left), FoldLocation(BoardLocation(101, c), Down))
          }
          // A right => C bottom
          for (r <- 151 to 200) {
            facings.addOne(FoldLocation(BoardLocation(r, 51), Right), FoldLocation(BoardLocation(150, r - 100), Up))
            facings.addOne(FoldLocation(BoardLocation(151, r - 100), Down), FoldLocation(BoardLocation(r, 50), Left))
          }
          // D right => F bottom
          for (r <- 51 to 100) {
            facings.addOne(FoldLocation(BoardLocation(r, 101), Right), FoldLocation(BoardLocation(50, 50 + r), Up))
            facings.addOne(FoldLocation(BoardLocation(51, 50 + r), Down), FoldLocation(BoardLocation(r, 100), Left))
          }
          // B left => E left
          for (r <- 101 to 150) {
            facings.addOne(FoldLocation(BoardLocation(r, 0), Left), FoldLocation(BoardLocation(151 - r, 51), Right))
            facings.addOne(FoldLocation(BoardLocation(151 - r, 50), Left), FoldLocation(BoardLocation(r, 1), Right))
          }
          // A left => E top
          for (r <- 151 to 200) {
            facings.addOne(FoldLocation(BoardLocation(r, 0), Left), FoldLocation(BoardLocation(1, r - 100), Down))
            facings.addOne(FoldLocation(BoardLocation(0, r - 100), Up), FoldLocation(BoardLocation(r, 1), Right))
          }
          // A bottom => F top
          for (c <- 1 to 50) {
            facings.addOne(FoldLocation(BoardLocation(201, c), Down), FoldLocation(BoardLocation(1, 100 + c), Down))
            facings.addOne(FoldLocation(BoardLocation(0, 100 + c), Up), FoldLocation(BoardLocation(200, c), Up))
          }
          // F right => C right
          for (r <- 1 to 50) {
            facings.addOne(FoldLocation(BoardLocation(r, 151), Right), FoldLocation(BoardLocation(151 - r, 100), Left))
            facings.addOne(FoldLocation(BoardLocation(151 - r, 101), Right), FoldLocation(BoardLocation(r, 150), Left))
          }
        }

        facings.toMap
      }))

    override def process(input: Input, context: Option[Context]): Option[Int] =
      context.map { ctx =>
        val facings = ctx.getFacings(input.board)
        val (location, facing) = walk(input.board, input.path, facings)
        val facingNumber = facing match {
          case Facing.Right => 0
          case Facing.Down  => 1
          case Facing.Left  => 2
          case Facing.Up    => 3
        }
        1000 * location.row + 4 * location.col + facingNumber
      }
  }

  override def run(args: List[String]): IO[ExitCode] = Runner.run(args)
}
