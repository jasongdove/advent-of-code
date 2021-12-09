package adventofcode.year2016

import adventofcode._
import adventofcode.utils._
import cats.effect._

object Day17 extends IOApp {

  case class Vault(position: Point, passcode: String, path: String) {
    private val open = Set('b', 'c', 'd', 'e', 'f')

    private val hash = (passcode + path).toMd5Hex
    private val up = open.contains(hash(0)) && position.y > 1
    private val down = open.contains(hash(1)) && position.y < 4
    private val left = open.contains(hash(2)) && position.x > 1
    private val right = open.contains(hash(3)) && position.x < 4

    def nextVaults = {
      val upVault =
        if (up) Some(Vault(Point(position.x, position.y - 1), passcode, path + "U")) else None
      val downVault =
        if (down) Some(Vault(Point(position.x, position.y + 1), passcode, path + "D")) else None
      val leftVault =
        if (left) Some(Vault(Point(position.x - 1, position.y), passcode, path + "L")) else None
      val rightVault =
        if (right) Some(Vault(Point(position.x + 1, position.y), passcode, path + "R")) else None

      (upVault ++ downVault ++ leftVault ++ rightVault).toList
    }
  }

  case class Input(passcode: String)

  case object Input {
    def from(input: List[String]): Input =
      Input(input.head)
  }

  case class Context(aggregate: List[String] => String)

  object Runner extends Day[Input, Context, String](2016, 17) {
    override def transformInput(lines: List[String]): Input =
      Input.from(lines)

    override def partOneContext(): Option[Context] =
      Some(Context(list => list.sortBy(_.length).head))

    override def partTwoContext(): Option[Context] =
      Some(Context(list => list.sortBy(_.length).last.length.toString))

    override def process(input: Input, context: Option[Context]): Option[String] =
      context.map { ctx =>
        @annotation.tailrec
        def loop(q: Vector[Vault], options: Vector[String]): List[String] =
          q match {
            case vault +: tail =>
              if (vault.position == Point(4, 4))
                loop(tail, options.appended(vault.path))
              else
                loop(tail ++ vault.nextVaults, options)
            case _ => options.toList
          }

        ctx.aggregate(loop(Vector(Vault(Point(1, 1), input.passcode, "")), Vector.empty))
      }
  }

  override def run(args: List[String]): IO[ExitCode] = Runner.run(args)
}
