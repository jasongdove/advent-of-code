package adventofcode.year2015

import adventofcode.Day
import cats.effect._

object Day5 extends IOApp {
  case class Context(classifier: String => Boolean)

  private def isNicePartOne(str: String): Boolean = {
    val vowels = Set('a', 'e', 'i', 'o', 'u')
    val badStrings = Set("ab", "cd", "pq", "xy")

    def threeVowels(str: String): Boolean = str.count(vowels.contains) >= 3

    def doubleLetter(str: String): Boolean = {
      @annotation.tailrec
      def loop(str: String): Boolean = {
        if (str.tail.isEmpty) false
        else if (str.head == str.tail.head) true
        else loop(str.tail)
      }
      loop(str)
    }

    def noBadStrings(str: String): Boolean =
      badStrings.forall(!str.contains(_))

    threeVowels(str) && doubleLetter(str) && noBadStrings(str)
  }

  private def isNicePartTwo(str: String): Boolean = {
    def notOverlappingPair(str: String): Boolean = {
      @annotation.tailrec
      def loop(str: String): Boolean = {
        if (str.length < 4) false
        else if (str.substring(2).contains(str.take(2))) true
        else loop(str.tail)
      }
      loop(str)
    }

    def repeatsWithSpace(str: String): Boolean = {
      @annotation.tailrec
      def loop(str: String): Boolean = {
        if (str.length < 3) false
        else if (str.head == str.charAt(2)) true
        else loop(str.tail)
      }
      loop(str)
    }

    notOverlappingPair(str) && repeatsWithSpace(str)
  }

  object Runner extends Day[List[String], Context, Long](2015, 5) {
    override def transformInput(lines: List[String]): List[String] = lines

    override def partOneContext(): Option[Context] =
      Some(Context(isNicePartOne))

    override def partTwoContext(): Option[Context] =
      Some(Context(isNicePartTwo))

    override def process(input: List[String], context: Option[Context]): Option[Long] =
      context.map { ctx =>
        input.count(ctx.classifier).toLong
      }
  }

  override def run(args: List[String]): IO[ExitCode] = Runner.run(args)
}
