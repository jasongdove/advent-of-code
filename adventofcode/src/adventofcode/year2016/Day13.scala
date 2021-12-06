package adventofcode.year2016

import adventofcode._
import cats.effect._

object Day13 extends IOApp {

  case class Input(favoriteNumber: Int)

  case object Input {
    def from(input: List[String]): Input =
      Input(input.head.toInt)
  }

  case class Context(process: Input => Int)

  case class Maze(favoriteNumber: Int) {
    def isOpen(p: Point) =
      ((p.x * p.x + 3 * p.x + 2 * p.x * p.y + p.y + p.y * p.y) + favoriteNumber).toBinaryString.count(_ == '1') % 2 == 0
  }

  object Runner extends Day[Input, Context, Int](2016, 13) {
    override def transformInput(lines: List[String]): Input =
      Input.from(lines)

    override def partOneContext(): Option[Context] =
      Some(Context(processPartOne(Point(31, 39))))

    override def partTwoContext(): Option[Context] =
      Some(Context(processPartTwo(50)))

    override def process(input: Input, context: Option[Context]): Option[Int] =
      context.map(_.process(input))

    private def processPartOne(target: Point)(input: Input): Int = {
      val maze = Maze(input.favoriteNumber)

      val q = scala.collection.mutable.Queue[(Point, Int)]((Point(1, 1), 0))

      @annotation.tailrec
      def loop(visited: List[Point]): Int = {
        val (current, dist) = q.dequeue()
        if (current == target) dist
        else {
          val nextPoints = current.adjacent
            .filter(p => p.x >= 0 && p.y >= 0)
            .filterNot(visited.contains)
            .filter(maze.isOpen)
          nextPoints.foreach(p => q.enqueue((p, dist + 1)))
          loop(visited ++ nextPoints)
        }
      }

      loop(List(Point(1, 1)))
    }

    private def processPartTwo(maxDistance: Int)(input: Input): Int = {
      val maze = Maze(input.favoriteNumber)

      val q = scala.collection.mutable.Queue[(Point, Int)]((Point(1, 1), 0))

      @annotation.tailrec
      def loop(matching: Int, visited: List[Point]): Int = {
        if (q.isEmpty) matching
        else {
          val (current, dist) = q.dequeue()
          val nextMatching = if (dist <= maxDistance) matching + 1 else matching
          val nextPoints = current.adjacent
            .filter(p => p.x >= 0 && p.y >= 0)
            .filter(_ => dist + 1 <= 50)
            .filterNot(visited.contains)
            .filter(maze.isOpen)
          nextPoints.foreach(p => q.enqueue((p, dist + 1)))
          loop(nextMatching, visited ++ nextPoints)
        }
      }

      loop(0, List(Point(1, 1)))
    }
  }

  override def run(args: List[String]): IO[ExitCode] = Runner.run(args)
}
