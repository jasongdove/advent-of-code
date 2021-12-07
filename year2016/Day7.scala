package adventofcode.year2016

import adventofcode.Day
import cats.effect._

object Day7 extends IOApp {
  case class IPAddress(supernetSequences: List[String], hypernetSequences: List[String]) {
    lazy val supportsTls: Boolean =
      hypernetSequences.forall(s => !containsAbba(s)) && supernetSequences.exists(s => containsAbba(s))
    lazy val supportsSsl: Boolean = hypernetSequences.exists(containsBab)

    private lazy val abas: List[String] =
      supernetSequences.flatMap(_.sliding(3).filter(isAba).toList)

    private def containsAbba(str: String): Boolean =
      str.sliding(4).exists(s => s.length == 4 && s(0) != s(1) && s(0) == s(3) && s(1) == s(2))

    private def containsBab(str: String): Boolean =
      str.sliding(3).exists(s => isAba(s) && abas.exists(a => a(0) == s(1) && a(1) == s(0)))

    private def isAba(str: String): Boolean =
      str.length == 3 && str(0) != str(1) && str(0) == str(2)
  }

  object IPAddress {
    private val bracketPattern = "\\[([a-z]+)\\]".r
    def from(line: String): IPAddress = {
      val hypernetSequences = bracketPattern.findAllMatchIn(line).map(_.group(0)).toList
      val supernetSequences = bracketPattern.replaceAllIn(line, _ => "_").split("_").toList
      IPAddress(supernetSequences, hypernetSequences)
    }
  }

  case class Context(aggregate: List[IPAddress] => Int)

  object Runner extends Day[List[IPAddress], Context, Int](2016, 7) {
    override def transformInput(lines: List[String]): List[IPAddress] =
      lines.map(IPAddress.from)

    override def partOneContext(): Option[Context] =
      Some(Context(_.count(_.supportsTls)))

    override def partTwoContext(): Option[Context] =
      Some(Context(_.count(_.supportsSsl)))

    override def process(input: List[IPAddress], context: Option[Context]): Option[Int] =
      context.map(_.aggregate(input))
  }

  override def run(args: List[String]): IO[ExitCode] = Runner.run(args)
}
