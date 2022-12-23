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

  aocTest(Day10.Runner, One, Example, 13140)
  aocTest(Day10.Runner, One, Real, 14540)

  aocTest(Day11.Runner, One, Example, 10605L)
  aocTest(Day11.Runner, One, Real, 56595L)
  aocTest(Day11.Runner, Two, Example, 2713310158L)
  aocTest(Day11.Runner, Two, Real, 15693274740L)

  aocTest(Day12.Runner, One, Example, 31)
  aocTest(Day12.Runner, One, Real, 361)
  aocTest(Day12.Runner, Two, Example, 29)
  aocTest(Day12.Runner, Two, Real, 354)

  aocTest(Day13.Runner, One, Example, 13)
  aocTest(Day13.Runner, One, Real, 6086)
  aocTest(Day13.Runner, Two, Example, 140)
  aocTest(Day13.Runner, Two, Real, 27930)

  aocTest(Day14.Runner, One, Example, 24)
  aocTest(Day14.Runner, One, Real, 843)
  aocTest(Day14.Runner, Two, Example, 93)
  aocTest(Day14.Runner, Two, Real, 27625)

  aocTest(Day15.Runner, One, Example, 26L)
  aocTest(Day15.Runner, One, Real, 5127797L)
  aocTest(Day15.Runner, Two, Example, 56000011L)
  aocTest(Day15.Runner, Two, Real, 12518502636475L)

  aocTest(Day16.Runner, One, Example, 1651L)
  aocTest(Day16.Runner, One, Real, 1792L)
  aocTest(Day16.Runner, Two, Example, 1707L)
  aocTest(Day16.Runner, Two, Real, 2587L)

  aocTest(Day17.Runner, One, Example, 3068L)
  aocTest(Day17.Runner, One, Real, 3217L)
  // aocTest(Day17.Runner, Two, Example, 1514285714288L)
  aocTest(Day17.Runner, Two, Real, 1585673352422L)

  aocTest(Day18.Runner, One, Example, 64)
  aocTest(Day18.Runner, One, Real, 4282)
  aocTest(Day18.Runner, Two, Example, 58)
  aocTest(Day18.Runner, Two, Real, 2452)

  aocTest(Day19.Runner, One, Example, 33)
  aocTest(Day19.Runner, One, Real, 1009)
  aocTest(Day19.Runner, Two, Example, 3472)
  aocTest(Day19.Runner, Two, Real, 18816)

  aocTest(Day20.Runner, One, Example, 3L)
  aocTest(Day20.Runner, One, Real, 2203L)
  aocTest(Day20.Runner, Two, Example, 1623178306L)
  aocTest(Day20.Runner, Two, Real, 6641234038999L)

  aocTest(Day21.Runner, One, Example, 152L)
  aocTest(Day21.Runner, One, Real, 85616733059734L)
  aocTest(Day21.Runner, Two, Example, 301L)
  aocTest(Day21.Runner, Two, Real, 3560324848168L)

  aocTest(Day22.Runner, One, Example, 6032)
  aocTest(Day22.Runner, One, Real, 75254)
  aocTest(Day22.Runner, Two, Example, 5031)
  aocTest(Day22.Runner, Two, Real, 108311)

  aocTest(Day23.Runner, One, Example, 110)
  aocTest(Day23.Runner, One, Real, 20)
  aocTest(Day23.Runner, Two, Example, 4241)
  aocTest(Day23.Runner, Two, Real, 1079)
}
