package adventofcode.year2020

import adventofcode.Day

case class Day19Rule(id: Int, value: String) {
  private val numPattern = ".*\\d.*".r
  val isExpanded: Boolean = !numPattern.matches(value)
}

object Day19Rule {
  private val letterPattern = """"([ab])"""".r
  private val pipePattern = ".*\\|.*".r

  def from(line: String): Day19Rule = {
    val split = line.split(": ")
    val id = split(0).toInt
    split(1) match {
      case letterPattern(letter) => Day19Rule(id, s" $letter ")
      case s @ pipePattern()     => Day19Rule(id, s" ( $s ) ")
      case s                     => Day19Rule(id, s" $s ")
    }
  }
}

case class Day19Data(rules: List[Day19Rule], messages: List[String])

object Day19Data {
  def from(lines: List[String]): Day19Data = {
    val rules = lines(0).split("\n").map(Day19Rule.from).sortBy(_.id).toList
    val messages = lines(1).split("\n").toList
    Day19Data(rules, messages)
  }
}

case class Day19Context(replacementRules: List[Day19Rule])

object Day19 extends Day[Day19Data, Day19Context, Int](2020, 19) {

  override def transformInput(lines: List[String]): Day19Data =
    Day19Data.from(lines)

  override def splitOn(): String = "\n\n"

  override def partOneContext(): Option[Day19Context] =
    Some(Day19Context(List.empty))

  override def partTwoContext(): Option[Day19Context] = {
    val replacementRules = List(
      // 8: 42 | 42 8
      // this repeats 42 as many times as needed
      Day19Rule(8, " 42 + "),
      // 11: 42 31 | 42 11 31
      // this is a little less straightforward since 42 and 31
      // need to be present the same number of times, but the repetition caps at 4
      Day19Rule(11, " ( 42 31 | 42 42 31 31 | 42 42 42 31 31 31 | 42 42 42 42 31 31 31 31 ) ")
    )

    Some(Day19Context(replacementRules))
  }

  override def process(input: Day19Data, context: Option[Day19Context]): Option[Int] =
    context.flatMap { ctx =>
      val rules = ctx.replacementRules.foldLeft(input.rules) { case (acc, rule) =>
        acc.updated(rule.id, rule)
      }

      val expandedRules = expandRules(rules).map { case Day19Rule(id, value) =>
        Day19Rule(id, value.replace(" ", ""))
      }

      expandedRules
        .find(_.id == 0)
        .map(z => input.messages.count(s => s.matches(z.value)))
    }

  private def expandRules(rules: List[Day19Rule]): List[Day19Rule] = {
    @annotation.tailrec
    def loop(acc: List[Day19Rule], expanded: List[Day19Rule]): List[Day19Rule] = {
      if (acc.length == expanded.length) acc
      else {
        val updated = expanded.foldLeft(acc) { case (acc, Day19Rule(id, value)) =>
          val target = s" $id "
          acc.map {
            case Day19Rule(i, v) if v.contains(target) =>
              Day19Rule(i, v.replace(target, value))
            case rule => rule
          }
        }
        loop(updated, updated.filter(_.isExpanded))
      }
    }

    loop(rules, rules.filter(_.isExpanded))
  }
}
