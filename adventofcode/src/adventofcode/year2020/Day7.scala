package adventofcode.year2020

import adventofcode.{Day, Edge, WeightedGraph}

case class Day7Context(process: WeightedGraph[String] => Int)

object Day7 extends Day[WeightedGraph[String], Day7Context, Int](2020, 7) {

  override def transformInput(lines: List[String]): WeightedGraph[String] =
    WeightedGraph.directedFrom(lines.flatMap(edgesFrom))

  override def partOneContext(): Option[Day7Context] =
    Some(Day7Context(processPartOne))

  override def partTwoContext(): Option[Day7Context] =
    Some(Day7Context(processPartTwo))

  override def process(input: WeightedGraph[String], context: Option[Day7Context]): Option[Int] =
    context.map(_.process(input))

  private def processPartOne(input: WeightedGraph[String]): Int =
    input.nodes.count(n => input.areConnected(n, "shiny gold"))

  private def processPartTwo(input: WeightedGraph[String]): Int = {
    def countBags(bag: String): Int = {
      input.adj.get(bag) match {
        case None => 0
        case Some(innerBags) =>
          innerBags.toList.map(b => b.weight + b.weight * countBags(b.value)).sum
      }
    }

    countBags("shiny gold")
  }

  private def edgesFrom(line: String): List[Edge[String]] = {
    val outerPattern = "(.*) bags contain (.*).".r
    val innerPattern = "(\\d+) ([a-z\\s]+) bag".r

    val outerPattern(color, canContainString) = line
    val canContain = innerPattern
      .findAllIn(canContainString)
      .foldLeft(Map[String, Int]()) { (m, s) =>
        val innerPattern(count, color) = s
        m + ((color, count.toInt))
      }

    canContain.map(cc => Edge(color, cc._1, cc._2)).toList
  }
}
