package adventofcode.year2015

import weaver._
import cats.effect._

object Day14Suite extends SimpleIOSuite {
  import Day14._

  simpleTest("part 1 example") {
    for {
      input <- exampleInput()
      result <- IO(process(input, Some(Day14Context(partOneScoring(1000)))))
      _ <- expect(result == Some(1120)).failFast
    } yield success
  }

  simpleTest("part 1 solution") {
    for {
      input <- realInput()
      result <- IO(process(input, partOneContext()))
      _ <- expect(result == Some(2640)).failFast
    } yield success
  }

  simpleTest("part 2 example") {
    for {
      input <- exampleInput()
      result <- IO(process(input, Some(Day14Context(partTwoScoring(1000)))))
      _ <- expect(result == Some(689)).failFast
    } yield success
  }

  simpleTest("part 2 solution") {
    for {
      input <- realInput()
      result <- IO(process(input, partTwoContext()))
      _ <- expect(result == Some(1102)).failFast
    } yield success
  }
}
