package adventofcode

import cats.effect._

sealed abstract class ResourceType
object ResourceType {
  case object Example extends ResourceType
  case object Real extends ResourceType
}

sealed abstract class PartNumber
object PartNumber {
  case object Unspecified extends PartNumber
  case object One extends PartNumber
  case object Two extends PartNumber
}

abstract class Day[A, B, C](val yearNumber: Int, val dayNumber: Int) extends IOApp {
  var partOneResult: Option[C] = None

  def splitOn(): String = "\n"

  def linesOfInput(resourceType: ResourceType, partNumber: PartNumber)(transform: List[String] => A): IO[A] = {
    val resourceTypeString = resourceType match {
      case ResourceType.Example => "-example"
      case ResourceType.Real    => ""
    }
    val partNumberString = partNumber match {
      case PartNumber.Unspecified => ""
      case PartNumber.One         => "-part1"
      case PartNumber.Two         => "-part2"
    }
    val resourceNameWithPartNumber = s"day$dayNumber$resourceTypeString$partNumberString.txt"
    linesOfInput(resourceNameWithPartNumber)(transform)
  }

  def linesOfInput(resourceName: String)(transform: List[String] => A): IO[A] =
    IO(transform(os.read(os.resource / yearNumber.toString / resourceName).split(splitOn()).toList))

  def transformInput(lines: List[String]): A
  def exampleInput(partNumber: PartNumber = PartNumber.Unspecified) =
    linesOfInput(ResourceType.Example, partNumber)(transformInput)
  def realInput(partNumber: PartNumber = PartNumber.Unspecified) =
    linesOfInput(ResourceType.Real, partNumber)(transformInput)

  def process(input: A, context: Option[B]): Option[C]

  def partOneContext(): Option[B] = None
  def partTwoContext(): Option[B] = None

  override def run(args: List[String]): IO[ExitCode] =
    for {
      input <- realInput()

      contextOne <- IO(partOneContext())
      resultOne <- IO(process(input, contextOne))
      _ <- IO(this.partOneResult = resultOne)
      _ <- IO(println(s"part 1: $resultOne"))

      contextTwo <- IO(partTwoContext())
      resultTwo <- IO(process(input, contextTwo))
      _ <- IO(println(s"part 2: $resultTwo"))
    } yield ExitCode.Success
}
