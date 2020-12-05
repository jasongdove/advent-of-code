package adventofcode.year2020

import weaver._
import cats.effect._

object Day4Suite extends SimpleIOSuite {
  import Day4._

  simpleTest("part 1 example") {
    for {
      input <- exampleInput()
      result <- IO(process(input, partOneContext()))
      _ <- expect(result == Some(2)).failFast
    } yield success
  }

  simpleTest("part 1 solution") {
    for {
      input <- realInput()
      result <- IO(process(input, partOneContext()))
      _ <- expect(result == Some(170)).failFast
    } yield success
  }

  simpleTest("part 2 example") {
    for {
      inputOne <- linesOfInput("day4-example-invalid.txt")(transformInput)
      resultOne <- IO(process(inputOne, partTwoContext()))
      _ <- expect(resultOne == Some(0)).failFast
      inputTwo <- linesOfInput("day4-example-valid.txt")(transformInput)
      resultTwo <- IO(process(inputTwo, partTwoContext()))
      _ <- expect(resultTwo == Some(4)).failFast
    } yield success
  }

  simpleTest("part 2 solution") {
    for {
      input <- realInput()
      result <- IO(process(input, partTwoContext()))
      _ <- expect(result == Some(103)).failFast
    } yield success
  }
}
