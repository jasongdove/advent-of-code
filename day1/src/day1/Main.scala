import cats.effect._

object Main extends IOApp {
  override def run(args: List[String]): IO[ExitCode] = {
    for {
      input <- readInput()
      one <- search(input, 2, 2020)
      _ <- printResult(one)
      two <- search(input, 3, 2020)
      _ <- printResult(two)
    } yield ExitCode.Success
  }

  def readInput(): IO[Set[Int]] =
    IO(os.read(os.resource / "input").split("\n").flatMap(_.toIntOption).toSet)

  def search(input: Set[Int], size: Int, total: Int): IO[Option[Set[Int]]] =
    IO(input.subsets(size).find(set => set.sum == total))

  def printResult(result: Option[Set[Int]]): IO[Unit] = {
    result match {
      case None      => IO(println("No result found"))
      case Some(set) => IO(println(s"${set.mkString(" * ")} = ${set.product}"))
    }
  }
}
