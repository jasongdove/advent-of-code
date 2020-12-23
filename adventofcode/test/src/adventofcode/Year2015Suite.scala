package adventofcode

import adventofcode.year2015._

object Year2015Suite extends BaseSuite {
  import adventofcode.PartNumber._
  import adventofcode.ResourceType._

  aocTest(Day13.Runner, One, Example, 330L)
  aocTest(Day13.Runner, One, Real, 709L)
  aocTest(Day13.Runner, Two, Real, 668L)

  aocTest(Day14.Runner, One, Example, Day14.Context(Day14.partOneScoring(1000)), 1120)
  aocTest(Day14.Runner, One, Real, 2640)
  aocTest(Day14.Runner, Two, Example, Day14.Context(Day14.partTwoScoring(1000)), 689)
  aocTest(Day14.Runner, Two, Real, 1102)

  aocTest(Day15.Runner, One, Example, 62842880L)
  aocTest(Day15.Runner, One, Real, 21367368L)
  aocTest(Day15.Runner, Two, Example, 57600000L)
  aocTest(Day15.Runner, Two, Real, 1766400L)

  aocTest(Day17.Runner, One, Example, Day17.Context(25, _.length), 4)
  aocTest(Day17.Runner, One, Real, 1304)
  aocTest(Day17.Runner, Two, Example, Day17.Context(25, Day17.minBucketOptions), 3)
  aocTest(Day17.Runner, Two, Real, 18)

  aocTest(Day18.Runner, One, Example, Day18.Context(4, identity), 4)
  aocTest(Day18.Runner, One, Real, 821)
  aocTest(Day18.Runner, Two, Real, 886)

  aocTest(Day19.Runner, One, Example, 4)
  aocTest(Day19.Runner, One, Real, 576)
}
