package aoc2020

import cats.effect._

object Day1 extends IOApp {
  override def run(args: List[String]): IO[ExitCode] = {
    for {
      input <- readInput()
      one <- IO(search(input, 2, 2020))
      _ <- printResult(one)
      two <- IO(search(input, 3, 2020))
      _ <- printResult(two)
    } yield ExitCode.Success
  }

  def parseInput(input: String): Set[Int] =
    input.split("\n").flatMap(_.toIntOption).toSet

  def readInput(): IO[Set[Int]] =
    IO(parseInput(os.read(os.resource / "day1.txt")))

  def search(input: Set[Int], size: Int, total: Int): Option[Set[Int]] =
    input.subsets(size).find(_.sum == total)

  def printResult(result: Option[Set[Int]]): IO[Unit] = {
    result match {
      case None      => IO(println("No result found"))
      case Some(set) => IO(println(s"${set.mkString(" * ")} = ${set.product}"))
    }
  }
}
