package adventofcode.year2021

import cats.effect._
import adventofcode._

object Day25 extends IOApp {

  case class Input(map: Grid[Char])

  case object Input {
    def from(input: List[String]): Input = {
      val rows = input.length
      val cols = input.head.length
      val data = for {
        (line, row) <- input.zipWithIndex
        (value, col) <- line.zipWithIndex
      } yield GridLocation(row, col) -> value
      Input(Grid(rows, cols, data.toMap))
    }
  }

  case class Context(process: Input => Long)

  object Runner extends Day[Input, Context, Long](2021, 25) {
    override def transformInput(lines: List[String]): Input =
      Input.from(lines)

    override def partOneContext(): Option[Context] =
      Some(Context(processPartOne))

    override def partTwoContext(): Option[Context] =
      Some(Context(processPartTwo))

    override def process(input: Input, context: Option[Context]): Option[Long] =
      context.map(_.process(input))

    private def processPartOne(input: Input): Long = {
      var map = input.map
      var num = 0
      var changed = true
      while (changed) {
        val result = step(map)
        map = result.map
        changed = result.changed
        num += 1
      }

      num.toLong
    }

    private def processPartTwo(input: Input): Long = {
      0L
    }

    case class StepResult(map: Grid[Char], changed: Boolean)

    def step(map: Grid[Char]): StepResult = {
      var changed = false
      var reference = map.copy()
      var working = map.copy()

      // move east
      for {
        r <- 0 until map.rows
        c <- 0 until map.columns
      } {
        map(r, c) match {
          case '>' =>
            if (reference.get(r, (c + 1) % map.columns).getOrElse('!') == '.') {
              working = working.updated(GridLocation(r, c), '.')
              working = working.updated(GridLocation(r, (c + 1) % map.columns), '>')
              changed = true
            }
          case _ => ()
        }
      }

      reference = working.copy()

      // move down
      for {
        r <- 0 until map.rows
        c <- 0 until map.columns
      } {
        map(r, c) match {
          case 'v' =>
            if (reference.get((r + 1) % map.rows, c).getOrElse('!') == '.') {
              working = working.updated(GridLocation(r, c), '.')
              working = working.updated(GridLocation((r + 1) % map.rows, c), 'v')
              changed = true
            }
          case _ => ()
        }
      }

      StepResult(working, changed)
    }
  }

  override def run(args: List[String]): IO[ExitCode] = Runner.run(args)
}
