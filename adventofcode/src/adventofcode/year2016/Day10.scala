package adventofcode.year2016

import adventofcode.Day

import scala.annotation.tailrec

case class Day10Bot(chips: Set[Int]) {
  val canProceed: Boolean = chips.size == 2
}

object Day10Bot {
  def low(bot: Day10Bot): Int = bot.chips.min

  def high(bot: Day10Bot): Int = bot.chips.max
}

sealed abstract class Day10Instruction

object Day10Instruction {
  case class Value(value: Int, target: Int) extends Day10Instruction
  case class Bot(source: Int, selector: Day10Bot => Int, target: Int) extends Day10Instruction
  case class BotOutput(source: Int, selector: Day10Bot => Int, target: Int) extends Day10Instruction

  private val valuePattern = "value (\\d+) goes to bot (\\d+)".r
  private val botToBotPattern = "bot (\\d+) gives low to bot (\\d+) and high to bot (\\d+)".r
  private val botToBotOutputPattern = "bot (\\d+) gives low to bot (\\d+) and high to output (\\d+)".r
  private val botToOutputBotPattern = "bot (\\d+) gives low to output (\\d+) and high to bot (\\d+)".r
  private val botToOutputPattern = "bot (\\d+) gives low to output (\\d+) and high to output (\\d+)".r

  def from(line: String): List[Day10Instruction] = {
    line match {
      case valuePattern(value, bot) => List(Value(value.toInt, bot.toInt))
      case botToBotPattern(source, lowBot, highBot) =>
        List(Bot(source.toInt, Day10Bot.low, lowBot.toInt), Bot(source.toInt, Day10Bot.high, highBot.toInt))
      case botToBotOutputPattern(source, lowBot, highBot) =>
        List(Bot(source.toInt, Day10Bot.low, lowBot.toInt), BotOutput(source.toInt, Day10Bot.high, highBot.toInt))
      case botToOutputBotPattern(source, lowBot, highBot) =>
        List(BotOutput(source.toInt, Day10Bot.low, lowBot.toInt), Bot(source.toInt, Day10Bot.high, highBot.toInt))
      case botToOutputPattern(source, lowBot, highBot) =>
        List(BotOutput(source.toInt, Day10Bot.low, lowBot.toInt), BotOutput(source.toInt, Day10Bot.high, highBot.toInt))
    }
  }
}

case class Day10Context(
  stopWhen: Map[Int, Day10Bot] => Option[Day10Bot],
  aggregate: (Map[Int, Day10Bot], Map[Int, Int], Option[Day10Bot]) => Option[Long]
)

object Day10 extends Day[List[Day10Instruction], Day10Context, Long](2016, 10) {

  override def transformInput(lines: List[String]): List[Day10Instruction] =
    lines.flatMap(Day10Instruction.from)

  override def partOneContext(): Option[Day10Context] =
    Some(
      Day10Context(
        _.find(kv => kv._2.chips == Set(61, 17)).map(_._2),
        (bots, _, result) => bots.find(b => result.contains(b._2)).map(_._1.toLong)
      )
    )

  override def partTwoContext(): Option[Day10Context] =
    Some(
      Day10Context(
        _ => None,
        (_, outputs, _) => Some(outputs(0).toLong * outputs(1) * outputs(2))
      )
    )

  override def process(input: List[Day10Instruction], context: Option[Day10Context]): Option[Long] =
    context.flatMap { ctx =>
      val values = input.collect { case v: Day10Instruction.Value => v }
      val instructions = input diff values
      val bots = values
        .foldLeft(Map[Int, Day10Bot]()) { case (bots, v) =>
          val chips = bots.get(v.target).map(_.chips).getOrElse(Set.empty)
          bots.updated(v.target, Day10Bot(chips + v.value))
        }
      val (finalBots, outputs, result) = evaluate(bots, ctx.stopWhen, instructions)
      ctx.aggregate(finalBots, outputs, result)
    }

  private def evaluate(
    startBots: Map[Int, Day10Bot],
    stopWhen: Map[Int, Day10Bot] => Option[Day10Bot],
    instructions: List[Day10Instruction]
  ): (Map[Int, Day10Bot], Map[Int, Int], Option[Day10Bot]) = {
    @tailrec
    def loop(
      bots: Map[Int, Day10Bot],
      outputs: Map[Int, Int],
      remaining: List[Day10Instruction]
    ): (Map[Int, Day10Bot], Map[Int, Int], Option[Day10Bot]) = {
      if (remaining.isEmpty) (bots, outputs, None)
      else {
        stopWhen(bots) match {
          case s: Some[Day10Bot] => (bots, outputs, s)
          case None =>
            val actionableBots = bots.filter(_._2.canProceed)
            val (resolvable, rest) = remaining.partition {
              case Day10Instruction.Bot(source, _, _)       => actionableBots.contains(source)
              case Day10Instruction.BotOutput(source, _, _) => actionableBots.contains(source)
              case Day10Instruction.Value(_, _)             => false
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
      botsOutputs: (Map[Int, Day10Bot], Map[Int, Int]),
      instruction: Day10Instruction
    ): (Map[Int, Day10Bot], Map[Int, Int]) = {
      val (bots, outputs) = botsOutputs
      instruction match {
        case Day10Instruction.Bot(source, selector, target) =>
          val bs = bots(source)
          val chips = bots.get(target).map(_.chips).getOrElse(Set.empty)
          val updatedTarget = bots.updated(target, Day10Bot(chips + selector(bs)))
          val updatedSource = updatedTarget.updated(source, Day10Bot(bs.chips - selector(bs)))
          (updatedSource, outputs)
        case Day10Instruction.BotOutput(source, selector, target) =>
          val bs = bots(source)
          val newOutputs = outputs + (target -> selector(bs))
          (bots, newOutputs)
        case _ =>
          (bots, outputs)
      }
    }

    loop(startBots, Map.empty, instructions)
  }
}
