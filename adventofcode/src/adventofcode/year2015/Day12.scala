package adventofcode.year2015

import adventofcode.Day
import cats.effect._

object Day12 extends IOApp {
  case class Context(filter: ujson.Value => Unit)

  private def filterRed(input: ujson.Value): Unit = {
    def traverse(v: ujson.Value): Boolean = v match {
      case a: ujson.Arr =>
        a.arr.filterInPlace(traverse)
        true
      case o: ujson.Obj =>
        o.obj.filterInPlace { case (_, v) => traverse(v) }
        o.obj.forall {
          case (_, s: ujson.Str) => s.str != "red"
          case _                 => true
        }
      case _ => true
    }
    traverse(input)
    ()
  }

  object Runner extends Day[ujson.Value, Context, Long](2015, 12) {

    override def transformInput(lines: List[String]): ujson.Value =
      ujson.read(lines.mkString)

    override def partOneContext(): Option[Context] =
      Some(Context(_ => ()))

    override def partTwoContext(): Option[Context] =
      Some(Context(filterRed))

    override def process(input: ujson.Value, context: Option[Context]): Option[Long] =
      context.map { ctx =>
        ctx.filter(input)

        def traverse(v: ujson.Value): Iterable[Long] = v match {
          case a: ujson.Arr => a.arr.flatMap(traverse)
          case o: ujson.Obj => o.obj.values.flatMap(traverse)
          case n: ujson.Num => Seq(n.value.toLong)
          case _            => Nil
        }

        val nums = traverse(input)
        nums.sum
      }
  }

  override def run(args: List[String]): IO[ExitCode] = Runner.run(args)
}
