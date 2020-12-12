package adventofcode.year2020

import weaver._
import cats.effect._

object Day12Suite extends SimpleIOSuite {
  import Day12._

  simpleTest("part 1 example") {
    for {
      input <- exampleInput()
      result <- IO(process(input, partOneContext()))
      _ <- expect(result == Some(25)).failFast
    } yield success
  }

  simpleTest("part 1 solution") {
    for {
      input <- realInput()
      result <- IO(process(input, partOneContext()))
      _ <- expect(result == Some(420)).failFast
    } yield success
  }

  simpleTest("part 2 example") {
    for {
      inputOne <- exampleInput()
      resultOne <- IO(process(inputOne, partTwoContext()))
      _ <- IO(Day11.partOneResult = resultOne)

      input <- exampleInput()
      result <- IO(process(input, partTwoContext()))
      _ <- expect(result == Some(286)).failFast
    } yield success
  }

  simpleTest("part 2 solution") {
    for {
      inputOne <- realInput()
      resultOne <- IO(process(inputOne, partOneContext()))
      _ <- IO(Day11.partOneResult = resultOne)

      input <- realInput()
      result <- IO(process(input, partTwoContext()))
      _ <- expect(result == Some(42073)).failFast
    } yield success
  }
}
