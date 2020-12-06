package adventofcode.year2015

import weaver._
import cats.effect._

object Day17Suite extends SimpleIOSuite {
  import Day17._

  simpleTest("part 1 example") {
    for {
      input <- exampleInput()
      result <- IO(process(input, Some(Day17Context(25, _.length))))
      _ <- expect(result == Some(4)).failFast
    } yield success
  }

  simpleTest("part 1 solution") {
    for {
      input <- realInput()
      result <- IO(process(input, partOneContext()))
      _ <- expect(result == Some(1304)).failFast
    } yield success
  }

  simpleTest("part 2 example") {
    for {
      input <- exampleInput()
      result <- IO(process(input, Some(Day17Context(25, minBucketOptions))))
      _ <- expect(result == Some(3)).failFast
    } yield success
  }

  simpleTest("part 2 solution") {
    for {
      input <- realInput()
      result <- IO(process(input, partTwoContext()))
      _ <- expect(result == Some(18)).failFast
    } yield success
  }
}
