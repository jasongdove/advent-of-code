package adventofcode.year2016

import adventofcode._
import adventofcode.utils._
import cats.effect._

object Day14 extends IOApp {

  case class Input(salt: String)

  case object Input {
    def from(input: List[String]): Input =
      Input(input.head)
  }

  case class Context(hashProcess: String => String)

  case class Hash(input: String, index: Int, hashProcess: String => String) {
    val hash = hashProcess(input.toMd5Hex)

    val threes = hash
      .sliding(3)
      .find(s => s.length == 3 && s(0) == s(1) && s(1) == s(2))
      .map(_.head)

    val fives = hash
      .sliding(5)
      .find(s => s.length == 5 && s(0) == s(1) && s(1) == s(2) && s(2) == s(3) && s(3) == s(4))
      .map(_.head)
  }

  object Runner extends Day[Input, Context, Int](2016, 14) {
    override def transformInput(lines: List[String]): Input =
      Input.from(lines)

    override def partOneContext(): Option[Context] =
      Some(Context(identity))

    override def partTwoContext(): Option[Context] =
      Some(Context(hash => (1 to 2016).toList.foldLeft(hash)((acc, _) => acc.toMd5Hex)))

    override def process(input: Input, context: Option[Context]): Option[Int] =
      context.map(ctx => process(input, ctx.hashProcess))

    private def process(input: Input, hashProcess: String => String): Int = {
      val allHashes = (0 to 25_000).map(i => Hash(input.salt + i, i, hashProcess))
      val keys = allHashes
        .filter(h =>
          h.threes match {
            case None        => false
            case Some(three) => allHashes.slice(h.index + 1, h.index + 1000).exists(_.fives.contains(three))
          }
        )
        .take(64)
        .toList

      keys.last.index
    }
  }

  override def run(args: List[String]): IO[ExitCode] = Runner.run(args)
}
