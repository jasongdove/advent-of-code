package adventofcode.year2016

import adventofcode.Day

case class Day4Room(encryptedName: String, sectorId: Int, checksum: List[Char]) {
  val isValid = {
    val checksumFrequencies = checksum.map(c => (c -> encryptedName.count(_ == c)))
    val encryptedFrequencies =
      encryptedName.groupMapReduce(identity)(_ => 1)(_ + _).toList.sortBy(f => (-f._2, f._1)).take(5)
    checksumFrequencies == encryptedFrequencies
  }

  val decryptedName = encryptedName.map { c =>
    c match {
      case '-' => ' '
      case _   => ('a' + ((c - 'a' + sectorId) % 26)).toChar
    }
  }.mkString
}
case class Day4Context(process: List[Day4Room] => Option[Int])

object Day4Room {
  val pattern = "([a-z\\-]+)-([0-9]+)\\[([a-z]{5})\\]".r
  def from(line: String): Day4Room = {
    val pattern(name, sector, checksum) = line
    Day4Room(name, sector.toInt, checksum.toList)
  }
}

object Day4 extends Day[List[Day4Room], Day4Context, Int](2016, 4) {
  override def transformInput(lines: List[String]): List[Day4Room] =
    lines.map(Day4Room.from)

  override def partOneContext(): Option[Day4Context] =
    Some(Day4Context(processPartOne))

  override def partTwoContext(): Option[Day4Context] =
    Some(Day4Context(processPartTwo))

  override def process(input: List[Day4Room], context: Option[Day4Context]): Option[Int] =
    context.flatMap(ctx => ctx.process(input))

  private def processPartOne(rooms: List[Day4Room]): Option[Int] =
    Some(rooms.filter(_.isValid).map(_.sectorId).sum)

  private def processPartTwo(rooms: List[Day4Room]): Option[Int] =
    rooms.find(_.decryptedName == "northpole object storage").map(_.sectorId)
}
