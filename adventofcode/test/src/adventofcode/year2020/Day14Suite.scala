package adventofcode.year2020

import weaver._
import cats.effect._
import _root_.adventofcode.PartNumber

object Day14Suite extends SimpleIOSuite {
  import Day14._

  simpleTest("part 1 example") {
    for {
      input <- exampleInput(PartNumber.One)
      result <- IO(process(input, partOneContext()))
      _ <- expect(result == Some(165)).failFast
    } yield success
  }

  simpleTest("part 1 solution") {
    for {
      input <- realInput()
      result <- IO(process(input, partOneContext()))
      _ <- expect(result == Some(6513443633260L)).failFast
    } yield success
  }

  simpleTest("part 2 example") {
    for {
      input <- exampleInput(PartNumber.Two)
      result <- IO(process(input, partTwoContext()))
      _ <- expect(result == Some(208)).failFast
    } yield success
  }

  simpleTest("part 2 solution") {
    for {
      input <- realInput()
      result <- IO(process(input, partTwoContext()))
      _ <- expect(result == Some(3442819875191L)).failFast
    } yield success
  }
}
