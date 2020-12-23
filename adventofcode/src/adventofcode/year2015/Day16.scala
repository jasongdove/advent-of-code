package adventofcode.year2015

import adventofcode.Day
import cats.effect._

object Day16 extends IOApp {
  private val referenceAunt =
    Aunt(None, Some(3), Some(7), Some(2), Some(3), Some(0), Some(0), Some(5), Some(3), Some(2), Some(1))

  case class Aunt(
    number: Option[Int],
    children: Option[Int],
    cats: Option[Int],
    samoyeds: Option[Int],
    pomeranians: Option[Int],
    akitas: Option[Int],
    vizslas: Option[Int],
    goldfish: Option[Int],
    trees: Option[Int],
    cars: Option[Int],
    perfumes: Option[Int]
  )

  object Aunt {
    def from(line: String): Aunt = {
      val outerPattern = "Sue (\\d+): (.*)".r
      val outerPattern(number, propertyString) = line
      val innerPattern = "(.*): (\\d+)".r
      val properties = propertyString
        .split(", ")
        .map { p =>
          val innerPattern(key, value) = p
          (key, value.toInt)
        }
        .toMap
      Aunt(
        Some(number.toInt),
        properties.get("children"),
        properties.get("cats"),
        properties.get("samoyeds"),
        properties.get("pomeranians"),
        properties.get("akitas"),
        properties.get("vizslas"),
        properties.get("goldfish"),
        properties.get("trees"),
        properties.get("cars"),
        properties.get("perfumes")
      )
    }
  }

  case class Context(score: Aunt => Int)


  def partOneCompare(aunt: Aunt): Int = {
    def score(selector: Aunt => Option[Int]) =
      selector(aunt) match {
        case s @ Some(_) => if (s == selector(referenceAunt)) 2 else 0
        case None        => 1
      }

    score(_.children) * score(_.cats) * score(_.samoyeds) * score(_.pomeranians) * score(_.akitas) +
      score(_.vizslas) * score(_.goldfish) * score(_.trees) * score(_.cars) * score(_.perfumes)
  }

  def partTwoCompare(aunt: Aunt): Int = {
    def score(selector: Aunt => Option[Int]) =
      selector(aunt) match {
        case s @ Some(_) => if (s == selector(referenceAunt)) 2 else 0
        case None        => 1
      }

    def minScore(selector: Aunt => Option[Int]) =
      selector(aunt) match {
        case Some(value) => if (selector(referenceAunt).exists(_ < value)) 2 else 0
        case None        => 1
      }

    def maxScore(selector: Aunt => Option[Int]) =
      selector(aunt) match {
        case Some(value) => if (selector(referenceAunt).exists(_ > value)) 2 else 0
        case None        => 1
      }

    score(_.children) * minScore(_.cats) * score(_.samoyeds) * maxScore(_.pomeranians) * score(_.akitas) +
      score(_.vizslas) * maxScore(_.goldfish) * minScore(_.trees) * score(_.cars) * score(_.perfumes)
  }

  object Runner extends Day[List[Aunt], Context, Int](2015, 16) {
    override def transformInput(lines: List[String]): List[Aunt] =
      lines.map(Aunt.from)

    override def partOneContext(): Option[Context] =
      Some(Context(partOneCompare))

    override def partTwoContext(): Option[Context] =
      Some(Context(partTwoCompare))

    override def process(input: List[Aunt], context: Option[Context]): Option[Int] = context.flatMap { ctx =>
      val result = input.map(a => (a, ctx.score(a))).sortWith(_._2 > _._2).head
      result._1.number
    }
  }

  override def run(args: List[String]): IO[ExitCode] = Runner.run(args)
}
