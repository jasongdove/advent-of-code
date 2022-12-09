//> using lib "com.disneystreaming::weaver-cats:0.7.7"

package adventofcode

import adventofcode.year2022._

object Year2022Suite extends BaseSuite {
  import adventofcode.PartNumber._
  import adventofcode.ResourceType._

  aocTest(Day1.Runner, One, Example, 24000)
  aocTest(Day1.Runner, One, Real, 71023)
  aocTest(Day1.Runner, Two, Example, 45000)
  aocTest(Day1.Runner, Two, Real, 206289)

  aocTest(Day2.Runner, One, Example, 15)
  aocTest(Day2.Runner, One, Real, 8890)
  aocTest(Day2.Runner, Two, Example, 12)
  aocTest(Day2.Runner, Two, Real, 10238)

  aocTest(Day3.Runner, One, Example, 157)
  aocTest(Day3.Runner, One, Real, 8105)
  aocTest(Day3.Runner, Two, Example, 70)
  aocTest(Day3.Runner, Two, Real, 2363)

  aocTest(Day4.Runner, One, Example, 2)
  aocTest(Day4.Runner, One, Real, 532)
  aocTest(Day4.Runner, Two, Example, 4)
  aocTest(Day4.Runner, Two, Real, 854)

  aocTest(Day5.Runner, One, Example, "CMZ")
  aocTest(Day5.Runner, One, Real, "SHQWSRBDL")
  aocTest(Day5.Runner, Two, Example, "MCD")
  aocTest(Day5.Runner, Two, Real, "CDTQZHBRS")

  aocTest(Day6.Runner, One, Example, 7)
  aocTest(Day6.Runner, One, Real, 1538)
  aocTest(Day6.Runner, Two, Example, 19)
  aocTest(Day6.Runner, Two, Real, 2315)

  aocTest(Day7.Runner, One, Example, 95437L)
  aocTest(Day7.Runner, One, Real, 1182909L)
  aocTest(Day7.Runner, Two, Example, 24933642L)
  aocTest(Day7.Runner, Two, Real, 2832508L)

  aocTest(Day8.Runner, One, Example, 21)
  aocTest(Day8.Runner, One, Real, 1792)
  aocTest(Day8.Runner, Two, Example, 8)
  aocTest(Day8.Runner, Two, Real, 334880)

  aocTest(Day9.Runner, One, Example, 13)
  aocTest(Day9.Runner, One, Real, 6367)
  aocTest(Day9.Runner, Two, Example, 1)
  aocTest(Day9.Runner, Two, Real, 2536)
}
