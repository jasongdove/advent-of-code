package aoc2020

import cats.effect._

abstract class Day[A](val dayNumber: Int) extends IOApp {
  private val exampleInputResourceName = s"day${dayNumber}-example.txt"
  private val realInputResourceName = s"day${dayNumber}.txt"

  def splitOn(): String = "\n"

  def linesOfInput(resourceName: String)(transform: List[String] => A): IO[A] =
    IO(transform(os.read(os.resource / resourceName).split(splitOn()).toList))

  def transformInput(lines: List[String]): A
  def exampleInput() = linesOfInput(exampleInputResourceName)(transformInput)
  def realInput() = linesOfInput(realInputResourceName)(transformInput)

  def transformInputOne(lines: List[String]): A = transformInput(lines)
  def transformInputTwo(lines: List[String]): A = transformInput(lines)
  def exampleInputOne() = linesOfInput(exampleInputResourceName)(transformInputOne)
  def realInputOne() = linesOfInput(realInputResourceName)(transformInputOne)
  def exampleInputTwo() = linesOfInput(exampleInputResourceName)(transformInputTwo)
  def realInputTwo() = linesOfInput(realInputResourceName)(transformInputTwo)
}
