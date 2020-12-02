object Main {
  def main(args: Array[String]) = {
    val input = os.read(os.resource / "input").split("\n").toList.flatMap(_.toIntOption)
    partOne(input)
    partTwo(input)
  }

  def partOne(input: List[Int]): Unit = {
    val result = findTwoNumsWithTotal(input, 2020)
      .map(nums => nums._1 * nums._2)
    println(result)
  }

  def partTwo(input: List[Int]): Unit = {
    val result = input
      .find(i => findTwoNumsWithTotal(input, 2020 - i).isDefined)
      .flatMap(i => {
        findTwoNumsWithTotal(input, 2020 - i).map(nums => (i, nums._1, nums._2))
      })
      .map(nums => nums._1 * nums._2 * nums._3)
    println(result)
  }

  def findTwoNumsWithTotal(input: List[Int], total: Int): Option[(Int, Int)] = {
    input.find(i => input.contains(total - i)).map(i => (i, total - i))
  }
}
