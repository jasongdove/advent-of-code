package adventofcode.year2022

import adventofcode.Day
import cats.effect._

object Day13 extends IOApp {
  sealed trait Element

  case class Lst(elements: List[Element]) extends Element {
    def append(element: Element): Lst = Lst(elements :+ element)
    def head: Element = elements.head
    def tail: Lst = Lst(elements.tail)
    override def toString: String =
      s"[${elements.mkString(",")}]"
  }

  case object Lst {
    def empty = Lst(List.empty)
    def from(e: Element) = Lst(List(e))
  }

  case class Val(v: Int) extends Element {
    override def toString: String = v.toString
  }

  object Element {
    def from(line: String): Lst = {
      def list(q: List[Char], v: Option[Int], lst: Lst): (List[Char], Lst) = {
        q match {
          case '[' :: next =>
            val thisList = list(next, None, Lst.empty)
            list(thisList._1, None, lst.append(thisList._2))
          case ']' :: next =>
            v match {
              case Some(value) => (next, lst.append(Val(value)))
              case None        => (next, lst)
            }
          case ',' :: next =>
            val lstWithVal = v match {
              case Some(value) => lst.append(Val(value))
              case None        => lst
            }
            list(next, None, lstWithVal)
          case head :: next =>
            val incoming = head.toString.toInt
            val nextV = v match {
              case Some(value) => value * 10 + incoming
              case None        => incoming
            }
            list(next, Some(nextV), lst)
          case Nil =>
            (List.empty, lst)
        }
      }

      list(line.toList.drop(1), None, Lst.empty)._2
    }
  }

  def compare(left: Lst, right: Lst): Option[Boolean] =
    (left.elements, right.elements) match {
      case (Val(l) :: lnext, Val(r) :: rnext) =>
        if (l == r) compare(Lst(lnext), Lst(rnext))
        else Some(l < r)
      case ((lhead: Lst) :: lnext, (rhead: Lst) :: rnext) =>
        compare(lhead, rhead).orElse(compare(Lst(lnext), Lst(rnext)))
      case ((lhead: Val) :: lnext, (rhead: Lst) :: rnext) =>
        compare(Lst(List(lhead)), rhead).orElse(compare(Lst(lnext), Lst(rnext)))
      case ((lhead: Lst) :: lnext, (rhead: Val) :: rnext) =>
        compare(lhead, Lst(List(rhead))).orElse(compare(Lst(lnext), Lst(rnext)))
      case (Nil, Nil) =>
        None
      case (Nil, _) =>
        Some(true)
      case (_, Nil) =>
        Some(false)
    }

  case class Context(solve: List[Lst] => Int)

  object Runner extends Day[List[Lst], Context, Int](2022, 13) {
    override def transformInput(lines: List[String]): List[Lst] =
      lines.filterNot(_.isEmpty).map(Element.from)

    override def partOneContext(): Option[Context] =
      Some(
        Context(
          _.sliding(2, 2).zipWithIndex
            .map { case (lists: List[Lst], index: Int) =>
              (compare(lists.head, lists.last).getOrElse(false), index + 1)
            }
            .filter(_._1 == true)
            .map(_._2)
            .sum
        )
      )

    override def partTwoContext(): Option[Context] =
      Some(Context(input => {
        val divider1 = Lst.from(Lst.from(Val(2)))
        val divider2 = Lst.from(Lst.from(Val(6)))
        val all = input.appendedAll(List(divider1, divider2))
        val sorted = all.sortWith((l, r) => compare(l, r).getOrElse(false))
        val index1 = sorted.indexOf(divider1) + 1
        val index2 = sorted.indexOf(divider2) + 1
        index1 * index2
      }))

    override def process(input: List[Lst], context: Option[Context]): Option[Int] =
      context.map(_.solve(input))
  }

  override def run(args: List[String]): IO[ExitCode] = Runner.run(args)
}
