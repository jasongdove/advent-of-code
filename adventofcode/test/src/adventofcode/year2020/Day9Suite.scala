package adventofcode.year2020

import weaver._
import cats.effect._

object Day9Suite extends SimpleIOSuite {
  import Day9._

  simpleTest("part 1 example") {
    for {
      input <- exampleInput()
      result <- IO(process(input, Some(Day9Context(processPartOne(5)))))
      _ <- expect(result == Some(127)).failFast
    } yield success
  }

  simpleTest("part 1 solution") {
    for {
      input <- realInput()
      result <- IO(process(input, partOneContext()))
      _ <- expect(result == Some(542529149)).failFast
    } yield success
  }

  simpleTest("part 2 example") {
    for {
      inputOne <- exampleInput()
      resultOne <- IO(process(inputOne, Some(Day9Context(processPartOne(5)))))
      _ <- IO(Day9.partOneResult = resultOne)

      input <- exampleInput()
      result <- IO(process(input, partTwoContext()))
      _ <- expect(result == Some(62)).failFast
    } yield success
  }

  simpleTest("part 2 solution") {
    for {
      inputOne <- realInput()
      resultOne <- IO(process(inputOne, partOneContext()))
      _ <- IO(Day9.partOneResult = resultOne)

      input <- realInput()
      result <- IO(process(input, partTwoContext()))
      _ <- expect(result == Some(75678618)).failFast
    } yield success
  }
}
