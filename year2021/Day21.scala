package adventofcode.year2021

import cats.effect._
import adventofcode._

object Day21 extends IOApp {

  case class Player(num: Int, space: Int, score: Int)

  case class Game(players: List[Player]) {
    private val playerQ = scala.collection.mutable.Queue.from(players)
    private var lastRoll = 0
    private var rollCounter = 0

    def isActive = playerQ.forall(_.score < 1000)
    def loser: Player = playerQ.sortBy(_.score).head

    def advance() {
      val player = playerQ.dequeue()

      val roll1 = roll()
      val roll2 = roll()
      val roll3 = roll()
      val nextSpace = (player.space + roll1 + roll2 + roll3 - 1) % 10 + 1
      val nextScore = player.score + nextSpace
      // println(s"Player ${player.num} rolls $roll1+$roll2+$roll3 and moves to space $nextSpace for a total score of $nextScore")

      val nextPlayer = player.copy(space = nextSpace, score = nextScore)
      playerQ.enqueue(nextPlayer)
    }

    def rollCount(): Int = rollCounter
    def roll(): Int = {
      rollCounter += 1
      lastRoll = (lastRoll % 100) + 1
      lastRoll
    }
  }

  case class Input(game: Game)

  case object Input {
    val pattern = "Player (\\d+) starting position: (\\d+)".r

    def from(input: List[String]): Input = {
      val players = input.map { line =>
        line match {
          case pattern(num, pos) => Player(num.toInt, pos.toInt, 0)
        }
      }
      Input(Game(players))
    }
  }

  case class Context(process: Input => Long)

  object Runner extends Day[Input, Context, Long](2021, 21) {
    override def transformInput(lines: List[String]): Input =
      Input.from(lines)

    override def partOneContext(): Option[Context] =
      Some(Context(processPartOne))

    override def partTwoContext(): Option[Context] =
      Some(Context(processPartTwo))

    override def process(input: Input, context: Option[Context]): Option[Long] =
      context.map(_.process(input))

    private def processPartOne(input: Input): Long = {
      while (input.game.isActive)
        input.game.advance()

      input.game.loser.score * input.game.rollCount()
    }

    private def processPartTwo(input: Input): Long = {
      var p1wins = 0L
      var p2wins = 0L

      def loop(p1: Int, p1pos: Int, p2: Int, p2pos: Int, isp1: Boolean, weight: Long): Unit = {
        if (p1 >= 21) p1wins += weight
        else if (p2 >= 21) p2wins += weight
        else {
          def weightedMove(roll: Int, w: Int) = {
            val pos = if (isp1) p1pos else p2pos
            val nextPos = (pos + roll - 1) % 10 + 1
            val nextP1 = if (isp1) p1 + nextPos else p1
            val nextP2 = if (!isp1) p2 + nextPos else p2
            val nextP1Pos = if (isp1) nextPos else p1pos
            val nextP2Pos = if (!isp1) nextPos else p2pos

            loop(nextP1, nextP1Pos, nextP2, nextP2Pos, !isp1, weight * w)
          }

          weightedMove(3, 1)
          weightedMove(4, 3)
          weightedMove(5, 6)
          weightedMove(6, 7)
          weightedMove(7, 6)
          weightedMove(8, 3)
          weightedMove(9, 1)
        }
      }

      loop(
        0,
        input.game.players.find(_.num == 1).head.space,
        0,
        input.game.players.find(_.num == 2).head.space,
        true,
        1
      )

      math.max(p1wins, p2wins)
    }
  }

  override def run(args: List[String]): IO[ExitCode] = Runner.run(args)
}
