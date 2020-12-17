package adventofcode.year2020

import adventofcode.Day

abstract class PasswordPolicy {
  def isValid(password: String): Boolean
}

case class PasswordPolicyOne(min: Int, max: Int, letter: Char) extends PasswordPolicy {
  override def isValid(password: String): Boolean = {
    val count = password.count(c => c == letter)
    count >= min && count <= max
  }
}

case class PasswordPolicyTwo(first: Int, second: Int, letter: Char) extends PasswordPolicy {
  override def isValid(password: String): Boolean =
    password(first - 1) == letter ^ password(second - 1) == letter
}

case class PasswordWithPolicy(password: String, policy: PasswordPolicy) {
  def isValid: Boolean = policy.isValid(password)
}

object PasswordWithPolicy {
  private val pattern = "([0-9]+)\\-([0-9]+) ([a-z]): ([a-z]+)".r

  def one(input: String): PasswordWithPolicy = {
    val pattern(min, max, letter, password) = input
    val policy = PasswordPolicyOne(min.toInt, max.toInt, letter.head)
    new PasswordWithPolicy(password, policy)
  }

  def two(input: String): PasswordWithPolicy = {
    val pattern(first, second, letter, password) = input
    val policy = PasswordPolicyTwo(first.toInt, second.toInt, letter.head)
    new PasswordWithPolicy(password, policy)
  }
}

case class Day2Context(policyParser: String => PasswordWithPolicy)

object Day2 extends Day[List[String], Day2Context, Long](2020, 2) {
  override def transformInput(lines: List[String]): List[String] = lines

  override def partOneContext(): Option[Day2Context] =
    Some(Day2Context(PasswordWithPolicy.one))

  override def partTwoContext(): Option[Day2Context] =
    Some(Day2Context(PasswordWithPolicy.two))

  override def process(input: List[String], context: Option[Day2Context]): Option[Long] =
    context.map { ctx =>
      input.map(ctx.policyParser).count(_.isValid).toLong
    }
}
