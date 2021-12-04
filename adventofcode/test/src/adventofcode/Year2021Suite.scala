package adventofcode

import adventofcode.year2021._

object Year2021Suite extends BaseSuite {
  import adventofcode.PartNumber._
  import adventofcode.ResourceType._

  aocTest(Day1.Runner, One, Example, 7)
  aocTest(Day1.Runner, One, Real, 1529)
  aocTest(Day1.Runner, Two, Example, 5)
  aocTest(Day1.Runner, Two, Real, 1567)

  aocTest(Day2.Runner, One, Example, 150)
  aocTest(Day2.Runner, One, Real, 1451208)
  aocTest(Day2.Runner, Two, Example, 900)
  aocTest(Day2.Runner, Two, Real, 1620141160)

  aocTest(Day3.Runner, One, Example, 198)
  aocTest(Day3.Runner, One, Real, 4006064)
  aocTest(Day3.Runner, Two, Example, 230)
  aocTest(Day3.Runner, Two, Real, 5941884)

  aocTest(Day4.Runner, One, Example, 4512)
  aocTest(Day4.Runner, One, Real, 23177)
  aocTest(Day4.Runner, Two, Example, 1924)
  aocTest(Day4.Runner, Two, Real, 6804)
}
