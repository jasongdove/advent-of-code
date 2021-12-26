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

  aocTest(Day9.Runner, One, Example, 15)
  aocTest(Day9.Runner, One, Real, 425)
  aocTest(Day9.Runner, Two, Example, 1134)
  aocTest(Day9.Runner, Two, Real, 1135260)

  aocTest(Day10.Runner, One, Example, 26397L)
  aocTest(Day10.Runner, One, Real, 296535L)
  aocTest(Day10.Runner, Two, Example, 288957L)
  aocTest(Day10.Runner, Two, Real, 4245130838L)

  aocTest(Day11.Runner, One, Example, 1656L)
  aocTest(Day11.Runner, One, Real, 1669L)
  aocTest(Day11.Runner, Two, Example, 195L)
  aocTest(Day11.Runner, Two, Real, 351L)

  aocTest(Day12.Runner, One, Example, 10L)
  aocTest(Day12.Runner, One, Real, 3410L)
  aocTest(Day12.Runner, Two, Example, 36L)
  aocTest(Day12.Runner, Two, Real, 98796L)

  aocTest(Day13.Runner, One, Example, 17L)
  aocTest(Day13.Runner, One, Real, 724L)

  aocTest(Day14.Runner, One, Example, 1588L)
  aocTest(Day14.Runner, One, Real, 3118L)
  aocTest(Day14.Runner, Two, Example, 2188189693529L)
  aocTest(Day14.Runner, Two, Real, 4332887448171L)

  aocTest(Day15.Runner, One, Example, 40L)
  aocTest(Day15.Runner, One, Real, 698L)
  aocTest(Day15.Runner, Two, Example, 315L)
  aocTest(Day15.Runner, Two, Real, 3022L)

  aocTest(Day16.Runner, One, Real, 996L)
  aocTest(Day16.Runner, Two, Real, 96257984154L)

  aocTest(Day17.Runner, One, Example, 45L)
  aocTest(Day17.Runner, One, Real, 12246L)
  aocTest(Day17.Runner, Two, Example, 112L)
  aocTest(Day17.Runner, Two, Real, 3528L)

  aocTest(Day18.Runner, One, Example, 4140L)
  aocTest(Day18.Runner, One, Real, 3216L)
  aocTest(Day18.Runner, Two, Example, 3993L)
  aocTest(Day18.Runner, Two, Real, 4643L)

  // TODO: slow af
  aocTest(Day19.Runner, One, Example, 79L)
  // aocTest(Day19.Runner, One, Real, 425L)
  aocTest(Day19.Runner, Two, Example, 3621L)
  // aocTest(Day19.Runner, Two, Real, 13354L)

  aocTest(Day20.Runner, One, Real, 5354L)
  aocTest(Day20.Runner, Two, Real, 18269L)

  aocTest(Day21.Runner, One, Example, 739785L)
  aocTest(Day21.Runner, One, Real, 671580L)
  aocTest(Day21.Runner, Two, Example, 444356092776315L)
  aocTest(Day21.Runner, Two, Real, 912857726749764L)

  aocTest(Day23.Runner, One, Example, 12521L)
  // TODO: slow af
  // aocTest(Day23.Runner, One, Real, 13455L)
  // aocTest(Day23.Runner, Two, Example, 44169L)
  aocTest(Day23.Runner, Two, Real, 43567L)

  aocTest(Day24.Runner, One, Real, 99691891979938L)
  aocTest(Day24.Runner, Two, Real, 27141191213911L)

  aocTest(Day25.Runner, One, Example, 58L)
  aocTest(Day25.Runner, One, Real, 571L)
}
