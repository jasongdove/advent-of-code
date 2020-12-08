package adventofcode.year2015

import adventofcode.Day

case class Wizard(hitPoints: Int, mana: Int, armor: Int) {
  def spendMana(m: Int) = Wizard(hitPoints, mana - m, armor)
  def takeDamage(damage: Int) = Wizard(hitPoints - Math.max(damage - armor, 1), mana, armor)
}

case class Day22Context(difficulty: Wizard => Wizard)

object Day22 extends Day[Boss, Day22Context, Int](2015, 22) {

  implicit class Day22Boss(boss: Boss) {
    def takeDamage(damage: Int) = Boss(boss.hitPoints - Math.max(damage - boss.defense, 1), boss.damage, boss.defense)
  }

  sealed abstract trait GameResult

  object GameResult {
    case object InProgress extends GameResult

    case object WizardDied extends GameResult
    case object BossDied extends GameResult
    case object OutOfMana extends GameResult

    case object EmptySpellQueue extends GameResult
    case object InvalidSpellCast extends GameResult
  }

  case class GameState(
    isWizardTurn: Boolean,
    wizard: Wizard,
    boss: Boss,
    activeTimers: List[SpellTimer],
    spellsCast: Vector[Spell],
    result: GameResult
  ) {
    def updated(w: Wizard, b: Boss): GameState = GameState(isWizardTurn, w, b, activeTimers, spellsCast, result)
    def updated(w: Wizard, timers: List[SpellTimer]): GameState =
      GameState(isWizardTurn, w, boss, timers, spellsCast, result)
    def completed(r: GameResult): GameState =
      GameState(isWizardTurn, wizard, boss, activeTimers, spellsCast, r)
    def difficultyTick(difficulty: Int): GameState = GameState(
      isWizardTurn,
      wizard.takeDamage(difficulty),
      boss,
      activeTimers,
      spellsCast,
      result
    )
  }

  sealed abstract class Spell(val mana: Int) {
    def onCast(wizard: Wizard): Wizard = wizard.spendMana(mana)
    def onCast(boss: Boss): Boss = boss
  }

  sealed abstract class Effect(mana: Int, val timer: Int) extends Spell(mana) {
    def tick(wizard: Wizard): Wizard = wizard
    def tick(boss: Boss): Boss = boss
    def onFade(wizard: Wizard): Wizard = wizard
  }

  object Spell {
    case object MagicMissile extends Spell(53) {
      override def onCast(boss: Boss): Boss = boss.takeDamage(4)
    }

    case object Drain extends Spell(73) {
      override def onCast(wizard: Wizard): Wizard =
        Wizard(wizard.hitPoints + 2, wizard.mana - mana, wizard.armor)
      override def onCast(boss: Boss): Boss = boss.takeDamage(2)
    }

    case object Shield extends Effect(113, 6) {
      override def onCast(wizard: Wizard): Wizard =
        Wizard(wizard.hitPoints, wizard.mana - mana, wizard.armor + 7)
      override def onFade(wizard: Wizard): Wizard =
        Wizard(wizard.hitPoints, wizard.mana, wizard.armor - 7)
    }

    case object Poison extends Effect(173, 6) {
      override def tick(boss: Boss): Boss = boss.takeDamage(3)
    }

    case object Recharge extends Effect(229, 5) {
      override def tick(wizard: Wizard): Wizard =
        Wizard(wizard.hitPoints, wizard.mana + 101, wizard.armor)
    }
  }

  case class SpellTimer(spell: Effect, timer: Int)

  override def transformInput(lines: List[String]): Boss = {
    val hpPattern = "Hit Points: (\\d+)".r
    val dmgPattern = "Damage: (\\d+)".r

    val hpPattern(hitPoints) = lines(0)
    val dmgPattern(damage) = lines(1)

    Boss(hitPoints.toInt, damage.toInt, 0)
  }

  override def partOneContext(): Option[Day22Context] =
    Some(Day22Context(identity))

  override def partTwoContext(): Option[Day22Context] =
    Some(Day22Context(_.takeDamage(1)))

  override def process(input: Boss, context: Option[Day22Context]): Option[Int] = {
    import GameResult._

    context.map { ctx =>
      val wizard = Wizard(50, 500, 0)
      val newGameState = GameState(true, wizard, input, List.empty, Vector.empty, GameResult.InProgress)

      generateSpellQueues().flatMap { sq =>
        val gameState = evaluate(newGameState, sq, ctx.difficulty)
        if (gameState.result == BossDied) Some(gameState.spellsCast.map(_.mana).sum)
        else None
      }.min
    }
  }

  private def generateSpellQueues(): Iterator[List[Spell]] = {
    import Spell._

    val size = 10

    List
      .fill(size)(List[Spell](MagicMissile, Drain, Shield, Poison, Recharge))
      .flatten
      .combinations(size)
      .flatMap(_.permutations)
  }

  private def evaluate(initialState: GameState, spellQueue: List[Spell], difficulty: Wizard => Wizard): GameState = {
    def applyTimers(state: GameState): GameState = {
      val nextWizard = state.activeTimers.foldLeft(state.wizard)((w, timer) => timer.spell.tick(w))
      val nextBoss = state.activeTimers.foldLeft(state.boss)((b, timer) => timer.spell.tick(b))

      (0 to state.activeTimers.length - 1).foldLeft(state.updated(nextWizard, nextBoss)) {
        case (s @ GameState(_, w, _, timers, _, _), timerIndex) =>
          val timer = timers(timerIndex)
          val nextTimer = SpellTimer(timer.spell, timer.timer - 1)
          val nextW = if (nextTimer.timer <= 0) timer.spell.onFade(w) else w
          s.updated(nextW, timers.updated(timerIndex, nextTimer).filter(_.timer > 0))
      }
    }

    @annotation.tailrec
    def playRound(
      state: GameState,
      spellQueue: List[Spell]
    ): GameState = {
      val difficultyState = if (state.isWizardTurn) state.updated(difficulty(state.wizard), state.boss) else state
      if (difficultyState.wizard.hitPoints <= 0) difficultyState.completed(GameResult.WizardDied)
      else {
        val nextState = applyTimers(difficultyState)
        if (nextState.wizard.hitPoints <= 0) nextState.completed(GameResult.WizardDied)
        else if (nextState.boss.hitPoints <= 0) nextState.completed(GameResult.BossDied)
        else {
          if (nextState.isWizardTurn) {
            spellQueue.headOption match {
              case None => nextState.completed(GameResult.EmptySpellQueue)
              case Some(e: Effect) =>
                if (nextState.activeTimers.exists(_.spell == e)) nextState.completed(GameResult.InvalidSpellCast)
                else if (nextState.wizard.mana < e.mana) nextState.completed(GameResult.OutOfMana)
                else {
                  playRound(
                    GameState(
                      !nextState.isWizardTurn,
                      e.onCast(nextState.wizard),
                      e.onCast(nextState.boss),
                      SpellTimer(e, e.timer) +: nextState.activeTimers,
                      nextState.spellsCast :+ e,
                      nextState.result
                    ),
                    spellQueue.tail
                  )
                }
              case Some(s) =>
                if (nextState.wizard.mana < s.mana) nextState.completed(GameResult.OutOfMana)
                else {
                  playRound(
                    GameState(
                      !nextState.isWizardTurn,
                      s.onCast(nextState.wizard),
                      s.onCast(nextState.boss),
                      nextState.activeTimers,
                      nextState.spellsCast :+ s,
                      nextState.result
                    ),
                    spellQueue.tail
                  )
                }
            }
          } else {
            playRound(
              GameState(
                !nextState.isWizardTurn,
                nextState.wizard.takeDamage(nextState.boss.damage),
                nextState.boss,
                nextState.activeTimers,
                nextState.spellsCast,
                nextState.result
              ),
              spellQueue
            )
          }
        }
      }
    }

    playRound(initialState, spellQueue)
  }
}
