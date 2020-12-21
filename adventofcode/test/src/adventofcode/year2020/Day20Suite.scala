package adventofcode.year2020

import cats.effect._
import weaver._

object Day20Suite extends SimpleIOSuite {
  import Day20._

  simpleTest("part 1 example") {
    for {
      input <- exampleInput()
      result <- IO(process(input, partOneContext()))
      _ <- expect(result.contains(20899048083289L)).failFast
    } yield success
  }

  simpleTest("part 1 solution") {
    for {
      input <- realInput()
      result <- IO(process(input, partOneContext()))
      _ <- expect(result.contains(104831106565027L)).failFast
    } yield success
  }

  simpleTest("part 2 example") {
    for {
      input <- exampleInput()
      result <- IO(process(input, partTwoContext()))
      _ <- expect(result.contains(273)).failFast
    } yield success
  }

  simpleTest("part 2 solution") {
    for {
      input <- realInput()
      result <- IO(process(input, partTwoContext()))
      _ <- expect(result.contains(2093)).failFast
    } yield success
  }
}
