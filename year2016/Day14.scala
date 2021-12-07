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

  case class Hash(input: String, hashProcess: String => String) {
    val hash = hashProcess(input.toMd5Hex)

    val threes = hash
      .sliding(3)
      .find(s => s(0) == s(1) && s(1) == s(2))
      .map(_.head)

    val fives = hash
      .sliding(5)
      .find(s => s(0) == s(1) && s(1) == s(2) && s(2) == s(3) && s(3) == s(4))
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
      @annotation.tailrec
      def loop(n: Int, threes: Map[Char, List[Int]], keys: Vector[Int]): Int = {
        if (keys.length >= 64) {
          // println(keys.length)
          // println(keys.sorted.zipWithIndex.map {
          //   case (i: Int, i2: Int) => {
          //     val hash = Hash(input.salt + i, hashProcess).hash
          //     s"${i2 + 1}: $i $hash"
          //   }
          // }.mkString("\n"))
          keys.sorted.toList(63)
        } else {
          val hash = Hash(input.salt + n, hashProcess)
          // if (n == 0)
          //   println(hash.hash)
          val keysToAdd = hash.fives
            .map { five =>
              val min = Integer.max(0, n - 1000)
              if (min < n) {
                (min until n)
                  .filterNot(keys.contains)
                  .filter(i => threes.get(five).isDefined && threes(five).contains(i))
                  .toSet
              } else Set.empty[Int]
            }
            .getOrElse(Set.empty)
            .toSet
          val nextThrees = hash.threes
            .map { c =>
              val current = threes.getOrElse(c, List.empty)
              threes.updated(c, current :+ n)
            }
            .getOrElse(threes)

          loop(n + 1, nextThrees, keys ++ keysToAdd)
        }
      }

      loop(0, Map.empty, Vector.empty)
    }
  }

  override def run(args: List[String]): IO[ExitCode] = Runner.run(args)
}
