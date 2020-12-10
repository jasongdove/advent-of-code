package adventofcode.year2020

import weaver._
import cats.effect._

object Day10Suite extends SimpleIOSuite {
  import Day10._

  simpleTest("part 1 example") {
    for {
      input <- exampleInput()
      result <- IO(process(input, partOneContext()))
      _ <- expect(result == Some(220)).failFast
    } yield success
  }

  simpleTest("part 1 solution") {
    for {
      input <- realInput()
      result <- IO(process(input, partOneContext()))
      _ <- expect(result == Some(1848)).failFast
    } yield success
  }

  simpleTest("part 2 example") {
    for {
      inputOne <- exampleInput()
      resultOne <- IO(process(inputOne, partTwoContext()))
      _ <- IO(Day10.partOneResult = resultOne)

      input <- exampleInput()
      result <- IO(process(input, partTwoContext()))
      _ <- expect(result == Some(19208)).failFast
    } yield success
  }

  simpleTest("part 2 solution") {
    for {
      inputOne <- realInput()
      resultOne <- IO(process(inputOne, partOneContext()))
      _ <- IO(Day10.partOneResult = resultOne)

      input <- realInput()
      result <- IO(process(input, partTwoContext()))
      _ <- expect(result == Some(8099130339328L)).failFast
    } yield success
  }
}
