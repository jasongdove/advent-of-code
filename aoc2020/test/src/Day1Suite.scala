package aoc2020

import weaver._
import cats.effect._

object Day1Suite extends SimpleIOSuite {
  simpleTest("part 1 example") {
    for {
      input <- Day1.readInput("day1-example.txt")
      result <- IO(Day1.search(input, 2, 2020))
      _ <- expect(result.map(_.product) == Some(514579)).failFast
    } yield success
  }

  simpleTest("part 1 solution") {
    for {
      input <- Day1.readInput("day1.txt")
      result <- IO(Day1.search(input, 2, 2020))
      _ <- expect(result.map(_.product) == Some(935419)).failFast
    } yield success
  }

  simpleTest("part 2 example") {
    for {
      input <- Day1.readInput("day1-example.txt")
      result <- IO(Day1.search(input, 3, 2020))
      _ <- expect(result.map(_.product) == Some(241861950)).failFast
    } yield success
  }

  simpleTest("part 2 solution") {
    for {
      input <- Day1.readInput("day1.txt")
      result <- IO(Day1.search(input, 3, 2020))
      _ <- expect(result.map(_.product) == Some(49880012)).failFast
    } yield success
  }
}
