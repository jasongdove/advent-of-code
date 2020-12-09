package adventofcode.year2015

import adventofcode.Day

object Day25 extends Day[(Int, Int), Nothing, Long](2015, 25) {

  override def transformInput(lines: List[String]): (Int, Int) = {
    val pattern = ".* row (\\d+), column (\\d+).$".r
    val pattern(row, column) = lines.mkString.trim
    (row.toInt, column.toInt)
  }

  override def process(input: (Int, Int), context: Option[Nothing]): Option[Long] =
      Some(codeForNumber((numberForCoordinates _).tupled(input)))

  private def numberForCoordinates(row: Int, col: Int): Int = {
    val rowStarts = LazyList.iterate((1, 1)) { case (num, acc) => (num + acc, acc + 1) }
    val r = rowStarts(row - 1)._1
    val colNumbers = LazyList.iterate((r, row + 1)) { case (num, acc) => (num + acc, acc + 1) }
    colNumbers(col - 1)._1
  }

  private def codeForNumber(number: Int): Long =
    LazyList.iterate(20151125L)(n => n * 252533 % 33554393)(number - 1)
}
