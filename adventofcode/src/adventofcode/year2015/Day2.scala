package adventofcode.year2015

import adventofcode.Day
import cats.effect._

object Day2 extends IOApp {
  case class Box(length: Long, width: Long, height: Long) {
    private val side1 = length * width
    private val side2 = width * height
    private val side3 = height * length
    private val smallestSide: Long = List(side1, side2, side3).min
    val requiredPaper: Long = 2 * side1 + 2 * side2 + 2 * side3 + smallestSide

    private val p1 = length * 2 + width * 2
    private val p2 = length * 2 + height * 2
    private val p3 = height * 2 + width * 2
    private val smallestPerimeter: Long = List(p1, p2, p3).min
    val requiredRibbon: Long = length * width * height + smallestPerimeter
  }

  object Box {
    private val pattern = "(\\d+)x(\\d+)x(\\d+)".r

    def from(line: String): Box = {
      val pattern(length, width, height) = line
      new Box(length.toLong, width.toLong, height.toLong)
    }
  }

  case class Context(process: List[Box] => Option[Long])

  private def processPartOne(input: List[Box]): Option[Long] =
    Some(input.foldLeft(0L)(_ + _.requiredPaper))

  private def processPartTwo(input: List[Box]): Option[Long] =
    Some(input.foldLeft(0L)(_ + _.requiredRibbon))

  object Runner extends Day[List[Box], Context, Long](2015, 2) {
    override def transformInput(lines: List[String]): List[Box] =
      lines.map(Box.from)

    override def partOneContext(): Option[Context] =
      Some(Context(processPartOne))

    override def partTwoContext(): Option[Context] =
      Some(Context(processPartTwo))

    override def process(input: List[Box], context: Option[Context]): Option[Long] =
      context.flatMap(_.process(input))
  }

  override def run(args: List[String]): IO[ExitCode] = Runner.run(args)
}
