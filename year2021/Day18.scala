package adventofcode.year2021

import cats.effect._
import adventofcode._

object Day18 extends IOApp {

  case class SnailfishNumber(number: String) {
    import SnailfishNumber.Token
    import SnailfishNumber.Token._

    def reduce(): SnailfishNumber = {
      val after = insideFour
      if (this == after) after.tenOrGreater else after
    }

    def magnitude(): Long = {
      val pattern = "\\[(\\d+),(\\d+)\\]".r("left", "right")

      var working = number

      while (pattern.findAllIn(working).size > 0) {
        working = pattern
          .findFirstMatchIn(working)
          .map(m => 3 * m.group("left").toLong + 2 * m.group("right").toLong)
          .map(m => pattern.replaceFirstIn(working, m.toString))
          .getOrElse(working)
      }

      working.toLong
    }

    private def tokens(): Vector[Token] =
      number
        .replaceAll("\\[", " [ ")
        .replaceAll("\\]", " ] ")
        .replaceAll(",", " , ")
        .split(" ")
        .filter(_.nonEmpty)
        .collect {
          case "["       => OpenParen
          case "]"       => CloseParen
          case ","       => Comma
          case v: String => Number(v.toInt)
        }
        .toVector

    private def insideFour(): SnailfishNumber = {
      @annotation.tailrec
      def loop(
        q: Vector[Token],
        acc: Vector[Token],
        depth: Int
      ): Vector[Token] = {
        q match {
          case head +: tail =>
            head match {
              case OpenParen =>
                loop(tail, acc :+ OpenParen, depth + 1)
              case Number(n) =>
                if (depth == 4 && acc.last == OpenParen) {
                  val accDrop = acc.dropRight(1)
                  val nextAcc = accDrop.findLast {
                    case Number(_) => true
                    case _         => false
                  } match {
                    case Some(Number(left)) =>
                      val index = accDrop.lastIndexOf(Number(left))
                      accDrop.updated(index, Number(left + n))
                    case _ => accDrop
                  }

                  val tailDrop = tail.drop(3)
                  val nextTail = tailDrop.find {
                    case Number(_) => true
                    case _         => false
                  } match {
                    case Some(Number(right)) =>
                      val rightN = tail(1) match {
                        case Number(n) => n
                        case _         => 0
                      }
                      val index = tailDrop.indexOf(Number(right))
                      tailDrop.updated(index, Number(right + rightN))
                    case _ => tailDrop
                  }

                  nextAcc ++ (Number(0) +: nextTail)
                } else loop(tail, acc :+ Number(n), depth)
              case CloseParen =>
                loop(tail, acc :+ CloseParen, depth - 1)
              case Comma =>
                loop(tail, acc :+ Comma, depth)
            }
          case _ => acc
        }
      }

      SnailfishNumber.from(loop(tokens, Vector.empty, -1))
    }

    private def tenOrGreater(): SnailfishNumber = {
      @annotation.tailrec
      def loop(
        q: Vector[Token],
        acc: Vector[Token]
      ): Vector[Token] = {
        q match {
          case head +: tail =>
            head match {
              case Number(n) if n >= 10 =>
                var left = Math.floor(n / 2.0).toInt
                var right = Math.ceil(n / 2.0).toInt
                acc ++ List(
                  OpenParen,
                  Number(left),
                  Comma,
                  Number(right),
                  CloseParen
                ) ++ tail
              case other =>
                loop(tail, acc :+ other)
            }
          case _ => acc
        }
      }

      SnailfishNumber.from(loop(tokens, Vector.empty))
    }
  }

  case object SnailfishNumber {
    sealed trait Token

    object Token {
      case class Number(value: Int) extends Token
      sealed trait Operator extends Token

      case object OpenParen extends Operator
      case object CloseParen extends Operator
      case object Comma extends Operator
    }

    def from(number: String) = SnailfishNumber(number)

    def from(tokens: Seq[Token]) =
      SnailfishNumber(tokens.map {
        case Token.OpenParen     => "["
        case Token.CloseParen    => "]"
        case Token.Comma         => ","
        case Token.Number(value) => value.toString
      }.mkString)

    def add(numbers: Seq[SnailfishNumber]): SnailfishNumber = {
      @annotation.tailrec
      def reduceLoop(number: SnailfishNumber): SnailfishNumber = {
        val reduced = number.reduce
        if (reduced == number) number else reduceLoop(reduced)
      }

      @annotation.tailrec
      def loop(q: Seq[SnailfishNumber]): SnailfishNumber = {
        q match {
          case head :: second :: tail =>
            val res = SnailfishNumber("[" + head.number + "," + second.number + "]")
            val red = reduceLoop(res)
            loop(List(red) ++ tail)
          case _ => q.head
        }
      }

      loop(numbers)
    }
  }

  case class Input(numbers: List[SnailfishNumber])

  case object Input {
    def from(input: List[String]): Input = {
      Input(input.map(SnailfishNumber.from))
    }
  }

  case class Context(process: Input => Long)

  object Runner extends Day[Input, Context, Long](2021, 18) {
    override def transformInput(lines: List[String]): Input =
      Input.from(lines)

    override def partOneContext(): Option[Context] =
      Some(Context(processPartOne))

    override def partTwoContext(): Option[Context] =
      Some(Context(processPartTwo))

    override def process(input: Input, context: Option[Context]): Option[Long] =
      context.map(_.process(input))

    private def processPartOne(input: Input): Long =
      SnailfishNumber.add(input.numbers).magnitude

    private def processPartTwo(input: Input): Long =
      input.numbers
        .combinations(2)
        .map { i => Math.max(SnailfishNumber.add(i).magnitude, SnailfishNumber.add(i.reverse).magnitude) }
        .max
  }

  override def run(args: List[String]): IO[ExitCode] = Runner.run(args)
}
