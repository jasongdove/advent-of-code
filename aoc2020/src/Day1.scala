package aoc2020

import cats.effect._

object Day1 extends IOApp {
  override def run(args: List[String]): IO[ExitCode] = {
    for {
      input <- readInput("day1.txt")
      one <- IO(search(input, 2, 2020))
      _ <- printResult(one)
      two <- IO(search(input, 3, 2020))
      _ <- printResult(two)
    } yield ExitCode.Success
  }

  def readInput(resourceName: String): IO[Set[Int]] =
    IO(os.read(os.resource / resourceName).split("\n").flatMap(_.toIntOption).toSet)

  def search(input: Set[Int], size: Int, total: Int): Option[Set[Int]] =
    input.subsets(size).find(_.sum == total)

  def printResult(result: Option[Set[Int]]): IO[Unit] = {
    result match {
      case None      => IO(println("No result found"))
      case Some(set) => IO(println(s"${set.mkString(" * ")} = ${set.product}"))
    }
  }
}
