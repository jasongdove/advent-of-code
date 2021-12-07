package adventofcode.year2016

import adventofcode.Day
import cats.effect._

object Day4 extends IOApp {
  case class Room(encryptedName: String, sectorId: Int, checksum: List[Char]) {
    val isValid: Boolean = {
      val checksumFrequencies = checksum.map(c => (c -> encryptedName.count(_ == c)))
      val encryptedFrequencies =
        encryptedName.groupMapReduce(identity)(_ => 1)(_ + _).toList.sortBy(f => (-f._2, f._1)).take(5)
      checksumFrequencies == encryptedFrequencies
    }

    val decryptedName: String = encryptedName.map { c =>
      c match {
        case '-' => ' '
        case _   => ('a' + ((c - 'a' + sectorId) % 26)).toChar
      }
    }.mkString
  }

  object Room {
    private val pattern = "([a-z\\-]+)-([0-9]+)\\[([a-z]{5})\\]".r
    def from(line: String): Room = {
      val pattern(name, sector, checksum) = line
      Room(name, sector.toInt, checksum.toList)
    }
  }

  case class Context(process: List[Room] => Option[Int])

  private def processPartOne(rooms: List[Room]): Option[Int] =
    Some(rooms.filter(_.isValid).map(_.sectorId).sum)

  private def processPartTwo(rooms: List[Room]): Option[Int] =
    rooms.find(_.decryptedName == "northpole object storage").map(_.sectorId)

  object Runner extends Day[List[Room], Context, Int](2016, 4) {
    override def transformInput(lines: List[String]): List[Room] =
      lines.map(Room.from)

    override def partOneContext(): Option[Context] =
      Some(Context(processPartOne))

    override def partTwoContext(): Option[Context] =
      Some(Context(processPartTwo))

    override def process(input: List[Room], context: Option[Context]): Option[Int] =
      context.flatMap(ctx => ctx.process(input))
  }

  override def run(args: List[String]): IO[ExitCode] = Runner.run(args)
}
