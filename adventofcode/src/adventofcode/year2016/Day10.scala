package adventofcode.year2016

import adventofcode.Day
import cats.effect._

import scala.annotation.tailrec

object Day10 extends IOApp {
  case class Bot(chips: Set[Int]) {
    val canProceed: Boolean = chips.size == 2
  }

  object Bot {
    def low(bot: Bot): Int = bot.chips.min
    def high(bot: Bot): Int = bot.chips.max
  }

  sealed trait Instruction

  object Instruction {
    case class ValueToBot(value: Int, target: Int) extends Instruction
    case class BotToBot(source: Int, selector: Bot => Int, target: Int) extends Instruction
    case class BotToOutput(source: Int, selector: Bot => Int, target: Int) extends Instruction

    private val valuePattern = "value (\\d+) goes to bot (\\d+)".r
    private val botToBotPattern = "bot (\\d+) gives low to bot (\\d+) and high to bot (\\d+)".r
    private val botToBotOutputPattern = "bot (\\d+) gives low to bot (\\d+) and high to output (\\d+)".r
    private val botToOutputBotPattern = "bot (\\d+) gives low to output (\\d+) and high to bot (\\d+)".r
    private val botToOutputPattern = "bot (\\d+) gives low to output (\\d+) and high to output (\\d+)".r

    def from(line: String): List[Instruction] = {
      line match {
        case valuePattern(value, bot) => List(ValueToBot(value.toInt, bot.toInt))
        case botToBotPattern(source, lowBot, highBot) =>
          List(BotToBot(source.toInt, Bot.low, lowBot.toInt), BotToBot(source.toInt, Bot.high, highBot.toInt))
        case botToBotOutputPattern(source, lowBot, highBot) =>
          List(BotToBot(source.toInt, Bot.low, lowBot.toInt), BotToOutput(source.toInt, Bot.high, highBot.toInt))
        case botToOutputBotPattern(source, lowBot, highBot) =>
          List(BotToOutput(source.toInt, Bot.low, lowBot.toInt), BotToBot(source.toInt, Bot.high, highBot.toInt))
        case botToOutputPattern(source, lowBot, highBot) =>
          List(
            BotToOutput(source.toInt, Bot.low, lowBot.toInt),
            BotToOutput(source.toInt, Bot.high, highBot.toInt)
          )
      }
    }
  }

  case class Context(
    stopWhen: Map[Int, Bot] => Option[Bot],
    aggregate: (Map[Int, Bot], Map[Int, Int], Option[Bot]) => Option[Int]
  )

  private def evaluate(
    startBots: Map[Int, Bot],
    stopWhen: Map[Int, Bot] => Option[Bot],
    instructions: List[Instruction]
  ): (Map[Int, Bot], Map[Int, Int], Option[Bot]) = {
    @tailrec
    def loop(
      bots: Map[Int, Bot],
      outputs: Map[Int, Int],
      remaining: List[Instruction]
    ): (Map[Int, Bot], Map[Int, Int], Option[Bot]) = {
      if (remaining.isEmpty) (bots, outputs, None)
      else {
        stopWhen(bots) match {
          case s: Some[Bot] => (bots, outputs, s)
          case None =>
            val actionableBots = bots.filter(_._2.canProceed)
            val (resolvable, rest) = remaining.partition {
              case Instruction.BotToBot(source, _, _)    => actionableBots.contains(source)
              case Instruction.BotToOutput(source, _, _) => actionableBots.contains(source)
              case Instruction.ValueToBot(_, _)          => false
            }
            if (resolvable.isEmpty) (bots, outputs, None)
            else {
              val (newBots, newOutputs) = resolvable.foldLeft(bots, outputs)(apply)
              loop(newBots, newOutputs, rest)
            }
        }
      }

    }

    def apply(
      botsOutputs: (Map[Int, Bot], Map[Int, Int]),
      instruction: Instruction
    ): (Map[Int, Bot], Map[Int, Int]) = {
      val (bots, outputs) = botsOutputs
      instruction match {
        case Instruction.BotToBot(source, selector, target) =>
          val bs = bots(source)
          val chips = bots.get(target).map(_.chips).getOrElse(Set.empty)
          val updatedTarget = bots.updated(target, Bot(chips + selector(bs)))
          val updatedSource = updatedTarget.updated(source, Bot(bs.chips - selector(bs)))
          (updatedSource, outputs)
        case Instruction.BotToOutput(source, selector, target) =>
          val bs = bots(source)
          val newOutputs = outputs + (target -> selector(bs))
          (bots, newOutputs)
        case _ =>
          (bots, outputs)
      }
    }

    loop(startBots, Map.empty, instructions)
  }

  object Runner extends Day[List[Instruction], Context, Int](2016, 10) {

    override def transformInput(lines: List[String]): List[Instruction] =
      lines.flatMap(Instruction.from)

    override def partOneContext(): Option[Context] =
      Some(
        Context(
          _.find(kv => kv._2.chips == Set(61, 17)).map(_._2),
          (bots, _, result) => bots.find(b => result.contains(b._2)).map(_._1)
        )
      )

    override def partTwoContext(): Option[Context] =
      Some(
        Context(
          _ => None,
          (_, outputs, _) => Some(outputs(0) * outputs(1) * outputs(2))
        )
      )

    override def process(input: List[Instruction], context: Option[Context]): Option[Int] =
      context.flatMap { ctx =>
        val values = input.collect { case v: Instruction.ValueToBot => v }
        val instructions = input diff values
        val bots = values
          .foldLeft(Map[Int, Bot]()) { case (bots, v) =>
            val chips = bots.get(v.target).map(_.chips).getOrElse(Set.empty)
            bots.updated(v.target, Bot(chips + v.value))
          }
        val (finalBots, outputs, result) = evaluate(bots, ctx.stopWhen, instructions)
        ctx.aggregate(finalBots, outputs, result)
      }
  }

  override def run(args: List[String]): IO[ExitCode] = Runner.run(args)
}
