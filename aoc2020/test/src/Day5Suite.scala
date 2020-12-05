package aoc2020

import weaver._
import cats.effect._

object Day5Suite extends SimpleIOSuite {
  simpleTest("part 1 example") {
    for {
      input <- Day5.readInput("day5-example.txt")
      result <- IO(Day5.maxId(input))
      _ <- expect(result == 820).failFast
    } yield success
  }

  simpleTest("part 1 solution") {
    for {
      input <- Day5.readInput("day5.txt")
      result <- IO(Day5.maxId(input))
      _ <- expect(result == 933).failFast
    } yield success
  }

  simpleTest("part 2 solution") {
    for {
      input <- Day5.readInput("day5.txt")
      result <- IO(Day5.mySeat(input))
      _ <- expect(result == Some(711)).failFast
    } yield success
  }
}
