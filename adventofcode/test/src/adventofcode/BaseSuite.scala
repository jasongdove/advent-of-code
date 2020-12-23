package adventofcode

import cats.effect.IO
import weaver._

abstract class BaseSuite extends SimpleIOSuite {
  def aocTest[A, B, C](
    day: Day[A, B, C],
    partNumber: PartNumber,
    resourceType: ResourceType,
    customContext: B,
    expectedResult: C
  ): Unit =
    aocTest(day, partNumber, resourceType, Some(customContext), expectedResult)

  def aocTest[A, B, C](
    day: Day[A, B, C],
    partNumber: PartNumber,
    resourceType: ResourceType,
    expectedResult: C
  ): Unit =
    aocTest(day, partNumber, resourceType, contextFor(day, partNumber), expectedResult)

  private def aocTest[A, B, C](
    day: Day[A, B, C],
    partNumber: PartNumber,
    resourceType: ResourceType,
    customContext: Option[B],
    expectedResult: C
  ): Unit = {
    val solutionOrExample = if (resourceType == ResourceType.Real) "solution" else "example"
    simpleTest(s"day ${day.dayNumber} part $partNumber $solutionOrExample") {
      for {
        input <- resourceFor(day, resourceType, partNumber)
        result <- IO(day.process(input, customContext))
        _ <- expect(result.contains(expectedResult)).failFast
      } yield success
    }
  }

  private def resourceFor[A, B, C](day: Day[A, B, C], resourceType: ResourceType, partNumber: PartNumber): IO[A] =
    resourceType match {
      case ResourceType.Example => day.exampleInput(partNumber).handleErrorWith(_ => day.exampleInput())
      case ResourceType.Real    => day.realInput()
    }

  private def contextFor[A, B, C](day: Day[A, B, C], partNumber: PartNumber): Option[B] =
    partNumber match {
      case PartNumber.One         => day.partOneContext()
      case PartNumber.Two         => day.partTwoContext()
      case PartNumber.Unspecified => None
    }
}
