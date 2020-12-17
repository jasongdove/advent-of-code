package adventofcode.year2020

import cats.effect._
import weaver._

object Day17Suite extends SimpleIOSuite {
  import Day17._

  simpleTest("part 1 example") {
    for {
      input <- exampleInput()
      result <- IO(process(input, partOneContext()))
      _ <- expect(result.contains(112)).failFast
    } yield success
  }

  simpleTest("part 1 solution") {
    for {
      input <- realInput()
      result <- IO(process(input, partOneContext()))
      _ <- expect(result.contains(304)).failFast
    } yield success
  }

  simpleTest("part 2 example") {
    for {
      input <- exampleInput()
      result <- IO(process(input, partTwoContext()))
      _ <- expect(result.contains(848)).failFast
    } yield success
  }

  simpleTest("part 2 solution") {
    for {
      input <- realInput()
      result <- IO(process(input, partTwoContext()))
      _ <- expect(result.contains(1868)).failFast
    } yield success
  }
}
