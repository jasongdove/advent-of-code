package aoc2020

import weaver._
import cats.effect._

object Day2Suite extends SimpleIOSuite {
  simpleTest("part 1 example") {
    for {
      input <- Day2.readInputOne("day2-example.txt")
      result <- IO(Day2.search(input))
      _ <- expect(result == 2).failFast
    } yield success
  }

  simpleTest("part 1 solution") {
    for {
      input <- Day2.readInputOne("day2.txt")
      result <- IO(Day2.search(input))
      _ <- expect(result == 477).failFast
    } yield success
  }

  simpleTest("part 2 example") {
    for {
      input <- Day2.readInputTwo("day2-example.txt")
      result <- IO(Day2.search(input))
      _ <- expect(result == 1).failFast
    } yield success
  }

  simpleTest("part 2 solution") {
    for {
      input <- Day2.readInputTwo("day2.txt")
      result <- IO(Day2.search(input))
      _ <- expect(result == 686).failFast
    } yield success
  }
}
