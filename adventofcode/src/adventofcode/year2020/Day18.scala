package adventofcode.year2020

import adventofcode.Day
import scala.annotation.tailrec

sealed trait Day18Token

object Day18Token {
  case class Number(value: Long) extends Day18Token
  sealed trait Operator extends Day18Token

  case object Add extends Operator
  case object Multiply extends Operator
  case object OpenParen extends Operator
  case object CloseParen extends Operator
}

case class Day18Context(precedence: Map[Day18Token.Operator, Int])

object Day18 extends Day[List[String], Day18Context, Long](2020, 18) {
  import Day18Token._

  override def transformInput(lines: List[String]): List[String] =
    lines

  override def partOneContext(): Option[Day18Context] =
    Some(Day18Context(Map.empty))

  override def partTwoContext(): Option[Day18Context] =
    Some(Day18Context(Map(Add -> 1, Multiply -> 0)))

  override def process(input: List[String], context: Option[Day18Context]): Option[Long] =
    context.map { ctx =>
      input
        .map(tokenize)
        .map(t => rpn(t, ctx.precedence))
        .map(evaluate)
        .sum
    }

  private def tokenize(expression: String): List[Day18Token] =
    expression
      .replaceAll("\\(", " ( ")
      .replaceAll("\\)", " ) ")
      .split(" ")
      .filter(_.nonEmpty)
      .collect {
        case "+"       => Add
        case "*"       => Multiply
        case "("       => OpenParen
        case ")"       => CloseParen
        case v: String => Number(v.toLong)
      }
      .toList

  private def rpn(tokens: List[Day18Token], precedence: Map[Operator, Int]): List[Day18Token] = {
    // from https://en.wikipedia.org/wiki/Shunting-yard_algorithm#The_algorithm_in_detail
    @tailrec
    def loop(output: Vector[Day18Token], operators: List[Operator], remaining: List[Day18Token]): List[Day18Token] = {
      remaining match {
        case Nil => (output ++ operators).toList
        case (n: Number) :: next =>
          loop(output :+ n, operators, next)
        case OpenParen :: next =>
          loop(output, OpenParen :: operators, next)
        case CloseParen :: next =>
          val toOutput = operators.takeWhile(op => op != OpenParen)
          val stack = operators.slice(operators.indexOf(OpenParen) + 1, operators.length)
          loop(output ++ toOutput, stack, next)
        case (o: Operator) :: next =>
          val toOutput = operators.takeWhile { op =>
            op != OpenParen && precedence.getOrElse(op, 0) >= precedence.getOrElse(o, 0)
          }
          val stack = o :: operators.slice(toOutput.length, operators.length)
          loop(output ++ toOutput, stack, next)
      }
    }
    loop(Vector.empty, List.empty, tokens)
  }

  private def evaluate(rpnTokens: List[Day18Token]): Long = {
    @tailrec
    def loop(acc: List[Long], remaining: List[Day18Token]): Long = {
      remaining match {
        case Nil =>
          acc.head
        case Number(value) :: next =>
          loop(value :: acc, next)
        case Add :: next =>
          val left :: right :: rem = acc
          loop((left + right) :: rem, next)
        case Multiply :: next =>
          val left :: right :: rem = acc
          loop((left * right) :: rem, next)
        case _ :: next =>
          loop(acc, next)
      }
    }
    loop(List.empty, rpnTokens)
  }
}
