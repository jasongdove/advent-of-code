package adventofcode

import adventofcode.year2015._
import cats.effect.IO

object Year2015Suite extends BaseSuite {
  import adventofcode.PartNumber._
  import adventofcode.ResourceType._

  aocTest(Day1.Runner, One, Real, 280)
  aocTest(Day1.Runner, Two, Real, 1797)

  aocTest(Day2.Runner, One, Real, 1588178)
  aocTest(Day2.Runner, Two, Real, 3783758)

  aocTest(Day3.Runner, One, Real, 2592)
  aocTest(Day3.Runner, Two, Real, 2360)

  aocTest(Day4.Runner, One, Real, 282749)
  aocTest(Day4.Runner, Two, Real, 9962624)

  aocTest(Day5.Runner, One, Real, 258)
  aocTest(Day5.Runner, Two, Real, 53)

  aocTest(Day6.Runner, One, Real, 400410)
  aocTest(Day6.Runner, Two, Real, 15343601)

  aocTest(Day7.Runner, One, Real, 16076)
  simpleTest("day 7 part 2 solution") {
    import Day7.Runner._
    for {
      inputOne <- realInput()
      resultOne <- IO(process(inputOne, partOneContext()))
      _ <- IO(Day7.Runner.partOneResult = resultOne)

      input <- realInput()
      result <- IO(process(input, partTwoContext()))
      _ <- expect(result == Some(2797)).failFast
    } yield success
  }

  aocTest(Day8.Runner, One, Real, 1342)
  aocTest(Day8.Runner, Two, Real, 2074)

  aocTest(Day9.Runner, One, Real, 141)
  aocTest(Day9.Runner, Two, Real, 736)

  aocTest(Day10.Runner, One, Real, 329356)
  aocTest(Day10.Runner, Two, Real, 4666278)

  aocTest(Day11.Runner, One, Real, "hxbxxyzz")
  aocTest(Day11.Runner, Two, Real, "hxcaabcc")

  aocTest(Day12.Runner, One, Real, 111754)
  aocTest(Day12.Runner, Two, Real, 65402)

  aocTest(Day13.Runner, One, Example, 330)
  aocTest(Day13.Runner, One, Real, 709)
  aocTest(Day13.Runner, Two, Real, 668)

  aocTest(Day14.Runner, One, Example, Day14.Context(Day14.partOneScoring(1000)), 1120)
  aocTest(Day14.Runner, One, Real, 2640)
  aocTest(Day14.Runner, Two, Example, Day14.Context(Day14.partTwoScoring(1000)), 689)
  aocTest(Day14.Runner, Two, Real, 1102)

  aocTest(Day15.Runner, One, Example, 62842880)
  aocTest(Day15.Runner, One, Real, 21367368)
  aocTest(Day15.Runner, Two, Example, 57600000)
  aocTest(Day15.Runner, Two, Real, 1766400)

  aocTest(Day16.Runner, One, Real, 103)
  aocTest(Day16.Runner, Two, Real, 405)

  aocTest(Day17.Runner, One, Example, Day17.Context(25, _.length), 4)
  aocTest(Day17.Runner, One, Real, 1304)
  aocTest(Day17.Runner, Two, Example, Day17.Context(25, Day17.minBucketOptions), 3)
  aocTest(Day17.Runner, Two, Real, 18)

  aocTest(Day18.Runner, One, Example, Day18.Context(4, identity), 4)
  aocTest(Day18.Runner, One, Real, 821)
  aocTest(Day18.Runner, Two, Real, 886)

  aocTest(Day19.Runner, One, Example, 4)
  aocTest(Day19.Runner, One, Real, 576)

  aocTest(Day20.Runner, One, Real, 786240)
  aocTest(Day20.Runner, Two, Real, 831600)

  aocTest(Day21.Runner, One, Real, 78)
  aocTest(Day21.Runner, Two, Real, 148)

  aocTest(Day22.Runner, One, Real, 900)
  aocTest(Day22.Runner, Two, Real, 1216)

  aocTest(Day23.Runner, One, Real, 184)
  aocTest(Day23.Runner, Two, Real, 231)

  aocTest(Day24.Runner, One, Real, 11266889531L)
  aocTest(Day24.Runner, Two, Real, 77387711L)

  aocTest(Day25.Runner, One, Real, 19980801L)
}
