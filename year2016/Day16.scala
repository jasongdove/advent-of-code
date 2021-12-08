package adventofcode.year2016

import adventofcode._
import adventofcode.utils._
import cats.effect._

object Day16 extends IOApp {

  case class DragonCurve(input: String) {
    def iterate: DragonCurve = {
      val a = input
      val b = input.reverse.map {
        case '0' => '1'
        case '1' => '0'
        case s   => s
      }
      DragonCurve(a + '0' + b)
    }

    def checksum: String = {
      def check(s: String): String = s
        .grouped(2)
        .map(s =>
          s match {
            case "00" | "11" => "1"
            case "01" | "10" => "0"
            case _           => ""
          }
        )
        .mkString

      @annotation.tailrec
      def loop(acc: String): String =
        if (acc.length() % 2 == 1) acc
        else loop(check(acc))

      loop(check(input))
    }
  }

  case class Input(dragonCurve: DragonCurve)

  case object Input {
    def from(input: List[String]): Input =
      Input(DragonCurve(input.head))
  }

  case class Context(length: Int)

  object Runner extends Day[Input, Context, String](2016, 16) {
    override def transformInput(lines: List[String]): Input =
      Input.from(lines)

    override def partOneContext(): Option[Context] =
      Some(Context(272))

    override def partTwoContext(): Option[Context] =
      Some(Context(35651584))

    override def process(input: Input, context: Option[Context]): Option[String] =
      context.map(ctx => {
        @annotation.tailrec
        def loop(acc: DragonCurve): DragonCurve = {
          if (acc.input.length() >= ctx.length) DragonCurve(acc.input.take(ctx.length))
          else loop(acc.iterate)
        }

        loop(input.dragonCurve).checksum
      })
  }

  override def run(args: List[String]): IO[ExitCode] = Runner.run(args)
}
