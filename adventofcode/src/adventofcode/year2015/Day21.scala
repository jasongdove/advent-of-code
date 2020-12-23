package adventofcode.year2015

import adventofcode.Day
import cats.effect._

object Day21 extends IOApp {
  sealed trait Equipment {
    def name: String
    def cost: Int
    def damage: Int
    def armor: Int
  }

  case class Weapon(name: String, cost: Int, damage: Int, armor: Int) extends Equipment
  case class Armor(name: String, cost: Int, damage: Int, armor: Int) extends Equipment
  case class Ring(name: String, cost: Int, damage: Int, armor: Int) extends Equipment

  case class Player(hitPoints: Int, weapon: Weapon, armor: Option[Armor], rings: List[Ring]) {
    val damage: Int = weapon.damage + armor.map(_.damage).getOrElse(0) + rings.map(_.damage).sum
    val defense: Int = weapon.armor + armor.map(_.armor).getOrElse(0) + rings.map(_.armor).sum
    val cost: Int = weapon.cost + armor.map(_.cost).sum + rings.map(_.cost).sum
  }

  case class Boss(hitPoints: Int, damage: Int, defense: Int)

  object ItemShop {
    def weapons: Set[Weapon] = Set(
      Weapon("Dagger", 8, 4, 0),
      Weapon("Shortsword", 10, 5, 0),
      Weapon("Warhammer", 25, 6, 0),
      Weapon("Longsword", 40, 7, 0),
      Weapon("Greataxe", 74, 8, 0)
    )

    def armor: Set[Armor] = Set(
      Armor("Leather", 13, 0, 1),
      Armor("Chainmail", 31, 0, 2),
      Armor("Splintmail", 53, 0, 3),
      Armor("Bandedmail", 75, 0, 4),
      Armor("Platemail", 102, 0, 5)
    )

    def rings: Set[Ring] = Set(
      Ring("Damage +1", 25, 1, 0),
      Ring("Damage +2", 50, 2, 0),
      Ring("Damage +3", 100, 3, 0),
      Ring("Defense +1", 20, 0, 1),
      Ring("Defense +2", 40, 0, 2),
      Ring("Defense +3", 80, 0, 3)
    )
  }

  case class Context(aggregate: List[(Player, Boolean)] => Int)

  private def doesPlayerWin(player: Player, boss: Boss): Boolean = {
    @annotation.tailrec
    def playRound(playerHp: Int, bossHp: Int, isPlayerRound: Boolean): Boolean = {
      if (playerHp <= 0) false
      else if (bossHp <= 0) true
      else {
        val nextPlayerHp = if (isPlayerRound) playerHp else playerHp - boss.damage + player.defense
        val nextBossHp = if (isPlayerRound) bossHp - player.damage + boss.defense else bossHp
        playRound(nextPlayerHp, nextBossHp, !isPlayerRound)
      }
    }

    playRound(player.hitPoints, boss.hitPoints, isPlayerRound = true)
  }

  object Runner extends Day[Boss, Context, Int](2015, 21) {

    override def transformInput(lines: List[String]): Boss = {
      val hpPattern = "Hit Points: (\\d+)".r
      val dmgPattern = "Damage: (\\d+)".r
      val defPattern = "Armor: (\\d+)".r

      val hpPattern(hitPoints) = lines.head
      val dmgPattern(damage) = lines(1)
      val defPattern(defense) = lines(2)

      Boss(hitPoints.toInt, damage.toInt, defense.toInt)
    }

    override def partOneContext(): Option[Context] =
      Some(Context(_.filter(_._2).map(_._1.cost).min))

    override def partTwoContext(): Option[Context] =
      Some(Context(_.filter(!_._2).map(_._1.cost).max))

    override def process(input: Boss, context: Option[Context]): Option[Int] =
      context.map { ctx =>
        val weaponOptions: List[Weapon] = ItemShop.weapons.toList
        val armorOptions: List[Option[Armor]] = ItemShop.armor.map(Some(_)).toList ++ List(None)
        val ringOptions = (ItemShop.rings.map[Option[Ring]](Some(_)).toList ++ List(None, None)).combinations(2).toList

        val players = for {
          weapon <- weaponOptions
          armor <- armorOptions
          rings <- ringOptions
        } yield Player(100, weapon, armor, rings.flatten)

        ctx.aggregate(players.map(p => (p, doesPlayerWin(p, input))))
      }
  }

  override def run(args: List[String]): IO[ExitCode] = Runner.run(args)
}
