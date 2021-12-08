// using lib com.disneystreaming::weaver-cats:0.7.7

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

  aocTest(Day5.Runner, One, Example, 5)
  aocTest(Day5.Runner, One, Real, 6572)
  aocTest(Day5.Runner, Two, Example, 12)
  aocTest(Day5.Runner, Two, Real, 21466)

  aocTest(Day6.Runner, One, Example, 5934L)
  aocTest(Day6.Runner, One, Real, 376194L)
  aocTest(Day6.Runner, Two, Example, 26984457539L)
  aocTest(Day6.Runner, Two, Real, 1693022481538L)

  aocTest(Day7.Runner, One, Example, 37)
  aocTest(Day7.Runner, One, Real, 349812)
  aocTest(Day7.Runner, Two, Example, 168)
  aocTest(Day7.Runner, Two, Real, 99763899)

  aocTest(Day8.Runner, One, Example, 26)
  aocTest(Day8.Runner, One, Real, 274)
  aocTest(Day8.Runner, Two, Example, 61229)
  aocTest(Day8.Runner, Two, Real, 1012089)
}
