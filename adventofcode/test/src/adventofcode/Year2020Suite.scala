package adventofcode

import adventofcode.year2020._
import cats.effect.IO

object Year2020Suite extends BaseSuite {
  import adventofcode.PartNumber._
  import adventofcode.ResourceType._

  aocTest(Day1, One, Example, 514579L)
  aocTest(Day1, One, Real, 935419L)
  aocTest(Day1, Two, Example, 241861950L)
  aocTest(Day1, Two, Real, 49880012L)

  aocTest(Day2, One, Example, 2L)
  aocTest(Day2, One, Real, 477L)
  aocTest(Day2, Two, Example, 1L)
  aocTest(Day2, Two, Real, 686L)

  aocTest(Day3, One, Example, 7L)
  aocTest(Day3, One, Real, 156L)
  aocTest(Day3, Two, Example, 336L)
  aocTest(Day3, Two, Real, 3521829480L)

  aocTest(Day4, One, Example, 2L)
  aocTest(Day4, One, Real, 170L)
  test("day 4 part 2 example") {
    import Day4._
    for {
      inputOne <- linesOfInput("day4-example-invalid.txt")(transformInput)
      resultOne <- IO(process(inputOne, partTwoContext()))
      _ <- expect(resultOne == Some(0)).failFast
      inputTwo <- linesOfInput("day4-example-valid.txt")(transformInput)
      resultTwo <- IO(process(inputTwo, partTwoContext()))
      _ <- expect(resultTwo == Some(4)).failFast
    } yield success
  }
  aocTest(Day4, Two, Real, 103L)

  aocTest(Day5, One, Example, 820L)
  aocTest(Day5, One, Real, 933L)
  aocTest(Day5, Two, Real, 711L)

  aocTest(Day6, One, Example, 11L)
  aocTest(Day6, One, Real, 6530L)
  aocTest(Day6, Two, Example, 6L)
  aocTest(Day6, Two, Real, 3323L)

  aocTest(Day7, One, Example, 4)
  aocTest(Day7, One, Real, 229)
  aocTest(Day7, Two, Example, 126)
  aocTest(Day7, Two, Real, 6683)

  aocTest(Day8, One, Example, 5)
  aocTest(Day8, One, Real, 1832)
  aocTest(Day8, Two, Example, 8)
  aocTest(Day8, Two, Real, 662)

  aocTest(Day9, One, Example, Day9Context(Day9.processPartOne(5)), 127L)
  aocTest(Day9, One, Real, 542529149L)
  test("day 9 part 2 example") {
    import Day9._
    for {
      inputOne <- exampleInput()
      resultOne <- IO(process(inputOne, Some(Day9Context(processPartOne(5)))))
      _ <- IO(Day9.partOneResult = resultOne)

      input <- exampleInput()
      result <- IO(process(input, partTwoContext()))
      _ <- expect(result == Some(62)).failFast
    } yield success
  }

  test("day 9 part 2 solution") {
    import Day9._
    for {
      inputOne <- realInput()
      resultOne <- IO(process(inputOne, partOneContext()))
      _ <- IO(Day9.partOneResult = resultOne)

      input <- realInput()
      result <- IO(process(input, partTwoContext()))
      _ <- expect(result == Some(75678618)).failFast
    } yield success
  }

  aocTest(Day10, One, Example, 220L)
  aocTest(Day10, One, Real, 1848L)
  aocTest(Day10, Two, Example, 19208L)
  aocTest(Day10, Two, Real, 8099130339328L)

  aocTest(Day11, One, Example, 37)
  aocTest(Day11, One, Real, 2338)
  aocTest(Day11, Two, Example, 26)
  aocTest(Day11, Two, Real, 2134)

  aocTest(Day12, One, Example, 25)
  aocTest(Day12, One, Real, 420)
  aocTest(Day12, Two, Example, 286)
  aocTest(Day12, Two, Real, 42073)

  aocTest(Day13, One, Example, 295L)
  aocTest(Day13, One, Real, 156L)
  aocTest(Day13, Two, Example, 1068781L)
  aocTest(Day13, Two, Real, 404517869995362L)

  aocTest(Day14, One, Example, 165L)
  aocTest(Day14, One, Real, 6513443633260L)
  aocTest(Day14, Two, Example, 208L)
  aocTest(Day14, Two, Real, 3442819875191L)

  aocTest(Day15, One, Example, 436)
  aocTest(Day15, One, Real, 870)
  aocTest(Day15, Two, Example, 175594)
  aocTest(Day15, Two, Real, 9136)

  aocTest(Day16, One, Example, 71L)
  aocTest(Day16, One, Real, 20231L)
  aocTest(Day16, Two, Real, 1940065747861L)

  aocTest(Day17, One, Example, 112)
  aocTest(Day17, One, Real, 304)
  aocTest(Day17, Two, Example, 848)
  aocTest(Day17, Two, Real, 1868)

  aocTest(Day18, One, Example, 26386L)
  aocTest(Day18, One, Real, 1890866893020L)
  aocTest(Day18, Two, Example, 693942L)
  aocTest(Day18, Two, Real, 34646237037193L)

  aocTest(Day19, One, Example, 2)
  aocTest(Day19, One, Real, 162)
  aocTest(Day19, Two, Example, 12)
  aocTest(Day19, Two, Real, 267)

  aocTest(Day20, One, Example, 20899048083289L)
  aocTest(Day20, One, Real, 104831106565027L)
  aocTest(Day20, Two, Example, 273L)
  aocTest(Day20, Two, Real, 2093L)

  aocTest(Day21, One, Example, "5")
  aocTest(Day21, One, Real, "2150")
  aocTest(Day21, Two, Example, "mxmxvkd,sqjhc,fvjkl")
  aocTest(Day21, Two, Real, "vpzxk,bkgmcsx,qfzv,tjtgbf,rjdqt,hbnf,jspkl,hdcj")

  aocTest(Day22, One, Example, 306L)
  aocTest(Day22, One, Real, 32629L)
  aocTest(Day22, Two, Example, 291L)
  aocTest(Day22, Two, Real, 32519L)

  aocTest(Day23.Runner, One, Example, "67384529")
  aocTest(Day23.Runner, One, Real, "47598263")
  aocTest(Day23.Runner, Two, Example, "149245887792")
  aocTest(Day23.Runner, Two, Real, "248009574232")

  aocTest(Day24.Runner, One, Example, 10)
  aocTest(Day24.Runner, One, Real, 521)
  aocTest(Day24.Runner, Two, Example, 2208)
  aocTest(Day24.Runner, Two, Real, 4242)

  aocTest(Day25.Runner, One, Example, 14897079)
  aocTest(Day25.Runner, One, Real, 12929)
}
