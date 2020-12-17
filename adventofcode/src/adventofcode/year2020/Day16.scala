package adventofcode.year2020

import adventofcode.Day

case class Day16TicketField(field: String, range1: Range, range2: Range) {
  val isDeparture: Boolean = field.startsWith("departure")
  def containsValue(value: Int): Boolean = range1.contains(value) || range2.contains(value)
}

object Day16TicketField {
  private val pattern = "([a-z\\s]+): (\\d+)-(\\d+) or (\\d+)-(\\d+)".r
  def from(line: String): Day16TicketField = {
    val pattern(field, r11, r12, r21, r22) = line
    Day16TicketField(field, Range.inclusive(r11.toInt, r12.toInt), Range.inclusive(r21.toInt, r22.toInt))
  }
}

case class Day16Ticket(fieldValues: List[Int]) {
  def invalidFieldValues(fields: List[Day16TicketField]): List[Int] = {
    fieldValues.filterNot(fv => fields.exists(f => f.containsValue(fv)))
  }
}

object Day16Ticket {
  def from(line: String): Day16Ticket = {
    val values = line.split(",").map(_.toInt).toList
    Day16Ticket(values)
  }
}

case class Day16Notes(fields: List[Day16TicketField], myTicket: Day16Ticket, nearbyTickets: List[Day16Ticket])

object Day16Notes {
  def from(lines: List[String]): Day16Notes = {
    val fields = lines(0).split("\n").map(Day16TicketField.from).toList
    val myTicket = Day16Ticket.from(lines(1).split("\n").last)
    val nearbyTickets = lines(2).split("\n").tail.map(Day16Ticket.from).toList
    Day16Notes(fields, myTicket, nearbyTickets)
  }
}

case class Day16Context(process: Day16Notes => Long)

object Day16 extends Day[Day16Notes, Day16Context, Long](2020, 16) {

  override def transformInput(lines: List[String]): Day16Notes =
    Day16Notes.from(lines)

  override def splitOn(): String = "\n\n"

  override def partOneContext(): Option[Day16Context] =
    Some(Day16Context(processPartOne))

  override def partTwoContext(): Option[Day16Context] =
    Some(Day16Context(processPartTwo))

  override def process(input: Day16Notes, context: Option[Day16Context]): Option[Long] =
    context.map(_.process(input))

  private def processPartOne(input: Day16Notes): Long =
    input.nearbyTickets.flatMap(_.invalidFieldValues(input.fields)).sum.toLong

  private def processPartTwo(input: Day16Notes): Long = {
    val validTickets = input.nearbyTickets.filter(_.invalidFieldValues(input.fields).isEmpty)
    identifyFields(input.fields, validTickets)
      .filter(_._2.isDeparture)
      .map { case (index, _) =>
        input.myTicket.fieldValues(index).toLong
      }
      .product
  }

  private def identifyFields(
    fields: List[Day16TicketField],
    validTickets: List[Day16Ticket]
  ): Map[Int, Day16TicketField] = {
    val fieldOptions = fields.indices
      .map { index =>
        val possibleValues = validTickets.map(_.fieldValues(index))
        val possibleFields = fields.filter(f => possibleValues.forall(v => f.containsValue(v)))
        (index, possibleFields)
      }
      .sortBy(_._2.length)

    fieldOptions.foldLeft(Map[Int, Day16TicketField]()) { case (acc, (index, fieldOptions)) =>
      val field = (fieldOptions diff acc.values.toSeq).head
      acc.updated(index, field)
    }
  }
}
