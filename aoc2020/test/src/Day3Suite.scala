package aoc2020

import weaver._
import cats.effect._

object Day3Suite extends SimpleIOSuite {
  import Day3._

  simpleTest("part 1 example") {
    for {
      input <- exampleInput()
      result <- IO(process(input, partOneContext()))
      _ <- expect(result == Some(7)).failFast
    } yield success
  }

  simpleTest("part 1 solution") {
    for {
      input <- realInput()
      result <- IO(process(input, partOneContext()))
      _ <- expect(result == Some(156)).failFast
    } yield success
  }

  simpleTest("part 2 example") {
    for {
      input <- exampleInput()
      result <- IO(process(input, partTwoContext()))
      _ <- expect(result == Some(336)).failFast
    } yield success
  }

  simpleTest("part 2 solution") {
    for {
      input <- Day3.realInput()
      result <- IO(process(input, partTwoContext()))
      _ <- expect(result == Some(3521829480L)).failFast
    } yield success
  }
}
