package adventofcode.year2020

import weaver._
import cats.effect._

object Day16Suite extends SimpleIOSuite {
  import Day16._

  simpleTest("part 1 example") {
    for {
      input <- exampleInput()
      result <- IO(process(input, partOneContext()))
      _ <- expect(result == Some(71)).failFast
    } yield success
  }

  simpleTest("part 1 solution") {
    for {
      input <- realInput()
      result <- IO(process(input, partOneContext()))
      _ <- expect(result == Some(20231)).failFast
    } yield success
  }

  simpleTest("part 2 solution") {
    for {
      input <- realInput()
      result <- IO(process(input, partTwoContext()))
      _ <- expect(result == Some(1940065747861L)).failFast
    } yield success
  }
}
