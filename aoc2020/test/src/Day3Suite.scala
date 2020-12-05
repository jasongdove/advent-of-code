package aoc2020

import weaver._
import cats.effect._

object Day3Suite extends SimpleIOSuite {
  simpleTest("part 1 example") {
    for {
      input <- Day3.exampleInput()
      result <- IO(Day3.search(input, 3, 1))
      _ <- expect(result == 7).failFast
    } yield success
  }

  simpleTest("part 1 solution") {
    for {
      input <- Day3.realInput()
      result <- IO(Day3.search(input, 3, 1))
      _ <- expect(result == 156).failFast
    } yield success
  }

  simpleTest("part 2 example") {
    for {
      input <- Day3.exampleInput()
      result <- IO(Day3.multiSearch(input))
      _ <- expect(result == 336).failFast
    } yield success
  }

  simpleTest("part 2 solution") {
    for {
      input <- Day3.realInput()
      result <- IO(Day3.multiSearch(input))
      _ <- expect(result == 3521829480L).failFast
    } yield success
  }
}
