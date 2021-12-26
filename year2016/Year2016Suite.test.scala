// using lib com.disneystreaming::weaver-cats:0.7.7

package adventofcode

import adventofcode.year2016._

object Year2016Suite extends BaseSuite {
  import adventofcode.PartNumber._
  import adventofcode.ResourceType._

  aocTest(Day1.Runner, One, Real, 181)
  aocTest(Day1.Runner, Two, Real, 140)

  aocTest(Day2.Runner, One, Real, "73597")
  aocTest(Day2.Runner, Two, Real, "A47DA")

  aocTest(Day3.Runner, One, Real, 993)
  aocTest(Day3.Runner, Two, Real, 1849)

  aocTest(Day4.Runner, One, Real, 22503)
  aocTest(Day4.Runner, Two, Real, 991)

  aocTest(Day5.Runner, One, Real, "c6697b55")
  aocTest(Day5.Runner, Two, Real, "8c35d1ab")

  aocTest(Day6.Runner, One, Real, "mlncjgdg")
  aocTest(Day6.Runner, Two, Real, "bipjaytb")

  aocTest(Day7.Runner, One, Real, 115)
  aocTest(Day7.Runner, Two, Real, 231)

  aocTest(Day8.Runner, One, Real, 116)

  aocTest(Day9.Runner, One, Real, 115118L)
  aocTest(Day9.Runner, Two, Real, 11107527530L)

  aocTest(Day10.Runner, One, Real, 56)
  aocTest(Day10.Runner, Two, Real, 7847)

  aocTest(Day11.Runner, One, Real, 37)

  aocTest(Day12.Runner, One, Real, 318007)
  aocTest(Day12.Runner, Two, Real, 9227661)

  aocTest(Day13.Runner, One, Real, 82)
  aocTest(Day13.Runner, Two, Real, 138)

  aocTest(Day14.Runner, One, Example, 22728)
  aocTest(Day14.Runner, One, Real, 18626)
  aocTest(Day14.Runner, Two, Example, 22551)
  aocTest(Day14.Runner, Two, Real, 20092)

  aocTest(Day15.Runner, One, Real, 121834)
  aocTest(Day15.Runner, Two, Real, 3208099)

  aocTest(Day16.Runner, One, Real, "10010010110011010")
  aocTest(Day16.Runner, Two, Real, "01010100101011100")

  aocTest(Day17.Runner, One, Real, "RLDUDRDDRR")
  aocTest(Day17.Runner, Two, Real, "590")

  aocTest(Day18.Runner, One, Real, 1926L)
  aocTest(Day18.Runner, Two, Real, 19986699L)
}
