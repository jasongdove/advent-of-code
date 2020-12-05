package aoc2020

case class RequiredField(key: String, validator: String => Boolean)

object RequiredField {
  def anyValidator(s: String): Boolean = s == s

  def byrValidator(s: String): Boolean = s.toIntOption.map(i => i >= 1920 && i <= 2002).getOrElse(false)
  def iyrValidator(s: String): Boolean = s.toIntOption.map(i => i >= 2010 && i <= 2020).getOrElse(false)
  def eyrValidator(s: String): Boolean = s.toIntOption.map(i => i >= 2020 && i <= 2030).getOrElse(false)
  def hgtValidator(s: String): Boolean = {
    val validCm =
      s.endsWith("cm") && s.substring(0, s.length - 2).toIntOption.map(i => i >= 150 && i <= 193).getOrElse(false)
    val validIn =
      s.endsWith("in") && s.substring(0, s.length - 2).toIntOption.map(i => i >= 59 && i <= 76).getOrElse(false)
    validCm || validIn
  }

  private val hclPattern = "#[0-9a-f]{6}".r
  def hclValidator(s: String): Boolean = hclPattern.matches(s)

  private val eclValues = List("amb", "blu", "brn", "gry", "grn", "hzl", "oth")
  def eclValidator(s: String): Boolean = eclValues.contains(s)

  private val pidPattern = "^[0-9]{9}$".r
  def pidValidator(s: String): Boolean = pidPattern.matches(s)
}

case class PassportField(key: String, value: String) {
  def isValid(required: List[RequiredField]): Boolean =
    required
      .find(_.key == key)
      .map(_.validator(value))
      .getOrElse(false)
}

case class Passport(fields: List[PassportField])

object Passport {
  def from(line: String): Passport = {
    val fields = line
      .split(" ")
      .toList
      .map { s =>
        val sp = s.split(":")
        val key = sp(0)
        val value = sp(1)
        PassportField(key, value)
      }
    Passport(fields)
  }
}

case class Day4Context(required: List[RequiredField], optional: List[String])

object Day4 extends Day[List[Passport], Day4Context](4) {
  private val part1Required = List(
    RequiredField("byr", RequiredField.anyValidator),
    RequiredField("iyr", RequiredField.anyValidator),
    RequiredField("eyr", RequiredField.anyValidator),
    RequiredField("hgt", RequiredField.anyValidator),
    RequiredField("hcl", RequiredField.anyValidator),
    RequiredField("ecl", RequiredField.anyValidator),
    RequiredField("pid", RequiredField.anyValidator)
  )

  private val part2Required = List(
    RequiredField("byr", RequiredField.byrValidator),
    RequiredField("iyr", RequiredField.iyrValidator),
    RequiredField("eyr", RequiredField.eyrValidator),
    RequiredField("hgt", RequiredField.hgtValidator),
    RequiredField("hcl", RequiredField.hclValidator),
    RequiredField("ecl", RequiredField.eclValidator),
    RequiredField("pid", RequiredField.pidValidator)
  )

  private val optional = List("cid")

  override def splitOn(): String = "\n\n"

  override def transformInput(lines: List[String]): List[Passport] =
    lines.map(_.replace("\n", " ")).map(Passport.from)

  override def partOneContext(): Option[Day4Context] =
    Some(Day4Context(part1Required, optional))

  override def partTwoContext(): Option[Day4Context] =
    Some(Day4Context(part2Required, optional))

  override def process(passports: List[Passport], context: Option[Day4Context]): Option[Long] =
    context.map { ctx =>
      val allKnown = ctx.required.map(_.key) ++ optional

      passports.count { passport =>
        val requiredFields = passport.fields.filter(f => ctx.required.exists(_.key == f.key))
        val missingRequired = requiredFields.length != ctx.required.length
        val unknown = passport.fields.exists(f => !allKnown.exists(_ == f.key))

        !unknown && !missingRequired && requiredFields.count(_.isValid(ctx.required)) == requiredFields.length
      }.toLong
    }
}
