package adventofcode.year2015

import weaver._
import cats.effect._

object Day19Suite extends SimpleIOSuite {
  import Day19._

  simpleTest("part 1 example") {
    for {
      input <- exampleInput()
      result <- IO(process(input, partOneContext()))
      _ <- expect(result == Some(4)).failFast
    } yield success
  }

  simpleTest("part 1 solution") {
    for {
      input <- realInput()
      result <- IO(process(input, partOneContext()))
      _ <- expect(result == Some(576)).failFast
    } yield success
  }
}
