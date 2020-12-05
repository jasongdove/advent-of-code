package aoc2020

import cats.effect._

object Day1 extends Day[Set[Int]](1) with IOApp {
  override def run(args: List[String]): IO[ExitCode] = {
    for {
      input <- realInput()
      one <- IO(search(input, 2, 2020))
      _ <- printResult(one)
      two <- IO(search(input, 3, 2020))
      _ <- printResult(two)
    } yield ExitCode.Success
  }

  override def transformInput(lines: List[String]): Set[Int] =
    lines.map(_.toIntOption).flatten.toSet

  def search(input: Set[Int], size: Int, total: Int): Option[Set[Int]] =
    input.subsets(size).find(_.sum == total)

  def printResult(result: Option[Set[Int]]): IO[Unit] = {
    result match {
      case None      => IO(println("No result found"))
      case Some(set) => IO(println(s"${set.mkString(" * ")} = ${set.product}"))
    }
  }
}
