package aoc2020

import cats.effect._

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

object Day4 extends IOApp {
  val part1Required = List(
    RequiredField("byr", RequiredField.anyValidator),
    RequiredField("iyr", RequiredField.anyValidator),
    RequiredField("eyr", RequiredField.anyValidator),
    RequiredField("hgt", RequiredField.anyValidator),
    RequiredField("hcl", RequiredField.anyValidator),
    RequiredField("ecl", RequiredField.anyValidator),
    RequiredField("pid", RequiredField.anyValidator)
  )

  val part2Required = List(
    RequiredField("byr", RequiredField.byrValidator),
    RequiredField("iyr", RequiredField.iyrValidator),
    RequiredField("eyr", RequiredField.eyrValidator),
    RequiredField("hgt", RequiredField.hgtValidator),
    RequiredField("hcl", RequiredField.hclValidator),
    RequiredField("ecl", RequiredField.eclValidator),
    RequiredField("pid", RequiredField.pidValidator)
  )

  val optional = List("cid")

  override def run(args: List[String]): IO[ExitCode] = {
    for {
      input <- readInput("day4.txt")
      resultOne <- IO(search(input, part1Required, optional))
      _ <- printResult(resultOne)
      resultTwo <- IO(search(input, part2Required, optional))
      _ <- printResult(resultTwo)
    } yield ExitCode.Success
  }

  def readInput(resourceName: String): IO[List[Passport]] = IO {
    os.read(os.resource / resourceName)
      .split("\n\n")
      .toList
      .map(s => s.replace("\n", " "))
      .map(Passport.from)
  }

  def search(passports: List[Passport], required: List[RequiredField], optional: List[String]): Int = {
    val allKnown = required.map(_.key) ++ optional

    passports
      .count { passport =>
        val requiredFields = passport.fields.filter(f => required.exists(_.key == f.key))
        val missingRequired = requiredFields.length != required.length
        val unknown = passport.fields.exists(f => !allKnown.exists(_ == f.key))

        !unknown && !missingRequired && requiredFields.count(_.isValid(required)) == requiredFields.length
      }
  }

  def printResult(result: Int): IO[Unit] =
    IO(println(s"found $result valid passports"))
}
