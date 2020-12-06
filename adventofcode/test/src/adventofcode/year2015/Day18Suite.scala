package adventofcode.year2015

import weaver._
import cats.effect._

object Day18Suite extends SimpleIOSuite {
  import Day18._

  simpleTest("part 1 example") {
    for {
      input <- exampleInput()
      result <- IO(process(input, Some(Day18Context(4, identity))))
      _ <- expect(result == Some(4)).failFast
    } yield success
  }

  simpleTest("part 1 solution") {
    for {
      input <- realInput()
      result <- IO(process(input, partOneContext()))
      _ <- expect(result == Some(821)).failFast
    } yield success
  }

  simpleTest("part 2 solution") {
    for {
      input <- realInput()
      result <- IO(process(input, partTwoContext()))
      _ <- expect(result == Some(886)).failFast
    } yield success
  }
}
