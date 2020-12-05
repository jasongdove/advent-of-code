package adventofcode

import cats.effect._

abstract class Day[A, B](val yearNumber: Int, val dayNumber: Int) extends IOApp {
  private val exampleInputResourceName = s"day${dayNumber}-example.txt"
  private val realInputResourceName = s"day${dayNumber}.txt"

  def splitOn(): String = "\n"

  def linesOfInput(resourceName: String)(transform: List[String] => A): IO[A] =
    IO(transform(os.read(os.resource / yearNumber.toString / resourceName).split(splitOn()).toList))

  def transformInput(lines: List[String]): A
  def exampleInput() = linesOfInput(exampleInputResourceName)(transformInput)
  def realInput() = linesOfInput(realInputResourceName)(transformInput)

  def process(input: A, context: Option[B]): Option[Long]

  def partOneContext(): Option[B] = None
  def partTwoContext(): Option[B] = None

  override def run(args: List[String]): IO[ExitCode] =
    for {
      input <- realInput()

      contextOne <- IO(partOneContext())
      resultOne <- IO(process(input, contextOne))
      _ <- IO(println(s"part 1: $resultOne"))

      contextTwo <- IO(partTwoContext())
      resultTwo <- IO(process(input, contextTwo))
      _ <- IO(println(s"part 2: $resultTwo"))
    } yield ExitCode.Success
}
