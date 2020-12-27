package adventofcode.year2015

import adventofcode.Day
import cats.effect._

object Day2 extends IOApp {
  case class Box(length: Int, width: Int, height: Int) {
    private val side1 = length * width
    private val side2 = width * height
    private val side3 = height * length
    private val smallestSide: Int = List(side1, side2, side3).min
    val requiredPaper: Int = 2 * side1 + 2 * side2 + 2 * side3 + smallestSide

    private val p1 = length * 2 + width * 2
    private val p2 = length * 2 + height * 2
    private val p3 = height * 2 + width * 2
    private val smallestPerimeter: Int = List(p1, p2, p3).min
    val requiredRibbon: Int = length * width * height + smallestPerimeter
  }

  object Box {
    private val pattern = "(\\d+)x(\\d+)x(\\d+)".r

    def from(line: String): Box = {
      val pattern(length, width, height) = line
      new Box(length.toInt, width.toInt, height.toInt)
    }
  }

  case class Context(process: List[Box] => Option[Int])

  private def processPartOne(input: List[Box]): Option[Int] =
    Some(input.foldLeft(0)(_ + _.requiredPaper))

  private def processPartTwo(input: List[Box]): Option[Int] =
    Some(input.foldLeft(0)(_ + _.requiredRibbon))

  object Runner extends Day[List[Box], Context, Int](2015, 2) {
    override def transformInput(lines: List[String]): List[Box] =
      lines.map(Box.from)

    override def partOneContext(): Option[Context] =
      Some(Context(processPartOne))

    override def partTwoContext(): Option[Context] =
      Some(Context(processPartTwo))

    override def process(input: List[Box], context: Option[Context]): Option[Int] =
      context.flatMap(_.process(input))
  }

  override def run(args: List[String]): IO[ExitCode] = Runner.run(args)
}
