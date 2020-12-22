package adventofcode.year2020

import adventofcode.Day

sealed abstract class Day22Player {
  val deck: Vector[Int]
}

case class Day22PlayerOne(deck: Vector[Int]) extends Day22Player
case class Day22PlayerTwo(deck: Vector[Int]) extends Day22Player

case class Day22Players(one: Day22PlayerOne, two: Day22PlayerTwo)

case class Day22Context(determineWinner: (Day22PlayerOne, Day22PlayerTwo) => Day22Player)

object Day22 extends Day[Day22Players, Day22Context, Long](2020, 22) {
  override def transformInput(lines: List[String]): Day22Players = {
    val oneLines = lines.head.split("\n")
    val twoLines = lines.last.split("\n")

    val one = Day22PlayerOne(oneLines.tail.map(_.toInt).toVector)
    val two = Day22PlayerTwo(twoLines.tail.map(_.toInt).toVector)
    Day22Players(one, two)
  }

  override def splitOn(): String = "\n\n"

  override def partOneContext(): Option[Day22Context] =
    Some(Day22Context(winnerPartOne))

  override def partTwoContext(): Option[Day22Context] =
    Some(Day22Context(winnerPartTwo))

  override def process(input: Day22Players, context: Option[Day22Context]): Option[Long] =
    context.map { ctx =>
      val winner = play(ctx.determineWinner, Set.empty, input.one, input.two)
      winner.deck.reverse.zipWithIndex.map { case (card, index) => card * (index + 1L) }.sum
    }

  private def winnerPartOne(one: Day22PlayerOne, two: Day22PlayerTwo): Day22Player = {
    if (one.deck.head > two.deck.head) one else two
  }

  private def winnerPartTwo(one: Day22PlayerOne, two: Day22PlayerTwo): Day22Player = {
    val cardOne = one.deck.head
    val cardTwo = two.deck.head
    if (one.deck.tail.length >= cardOne && two.deck.tail.length >= cardTwo) {
      val subGameWinner = play(
        winnerPartTwo,
        Set.empty,
        Day22PlayerOne(one.deck.tail.take(cardOne)),
        Day22PlayerTwo(two.deck.tail.take(cardTwo))
      )
      subGameWinner match {
        case Day22PlayerOne(_) => one
        case Day22PlayerTwo(_) => two
      }
    } else if (cardOne > cardTwo) one
    else two
  }

  @annotation.tailrec
  private def play(
    determineWinner: (Day22PlayerOne, Day22PlayerTwo) => Day22Player,
    played: Set[(Day22Player, Day22Player)],
    one: Day22PlayerOne,
    two: Day22PlayerTwo
  ): Day22Player = {
    if (played.contains((one, two))) one
    else if (one.deck.isEmpty) two
    else if (two.deck.isEmpty) one
    else {
      val cardOne = one.deck.head
      val cardTwo = two.deck.head
      determineWinner(one, two) match {
        case Day22PlayerOne(_) =>
          play(
            determineWinner,
            played + ((one, two)),
            Day22PlayerOne(one.deck.tail ++ Seq(cardOne, cardTwo)),
            Day22PlayerTwo(two.deck.tail)
          )
        case Day22PlayerTwo(_) =>
          play(
            determineWinner,
            played + ((one, two)),
            Day22PlayerOne(one.deck.tail),
            Day22PlayerTwo(two.deck.tail ++ Seq(cardTwo, cardOne))
          )
      }
    }
  }
}
