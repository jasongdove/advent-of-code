package adventofcode

import adventofcode.year2021._

object Year2021Suite extends BaseSuite {
  import adventofcode.PartNumber._
  import adventofcode.ResourceType._

  aocTest(Day1, One, Example, 7)
  aocTest(Day1, One, Real, 1529)
  aocTest(Day1, Two, Example, 5)
  aocTest(Day1, Two, Real, 1567)
}
