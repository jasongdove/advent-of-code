package adventofcode.year2020

import weaver._
import cats.effect._

object Day15Suite extends SimpleIOSuite {
  import Day15._

  simpleTest("part 1 example") {
    for {
      input <- exampleInput()
      result <- IO(process(input, partOneContext()))
      _ <- expect(result == Some(436)).failFast
    } yield success
  }

  simpleTest("part 1 solution") {
    for {
      input <- realInput()
      result <- IO(process(input, partOneContext()))
      _ <- expect(result == Some(870)).failFast
    } yield success
  }

  simpleTest("part 2 example") {
    for {
      input <- exampleInput()
      result <- IO(process(input, partTwoContext()))
      _ <- expect(result == Some(175594)).failFast
    } yield success
  }

  simpleTest("part 2 solution") {
    for {
      input <- realInput()
      result <- IO(process(input, partTwoContext()))
      _ <- expect(result == Some(9136)).failFast
    } yield success
  }
}
