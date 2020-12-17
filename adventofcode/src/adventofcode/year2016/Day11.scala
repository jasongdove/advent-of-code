package adventofcode.year2016

import adventofcode.Day

sealed abstract class Day11Equipment {
  def id: String
}

object Day11Equipment {

  case class Microchip(id: String) extends Day11Equipment

  case class RTG(id: String) extends Day11Equipment

  def from(str: String): Day11Equipment = {
    if (str.contains("microchip")) Microchip(s"${str.head.toUpper}M")
    else RTG(s"${str.head.toUpper}G")
  }
}

case class Day11Floor(number: Int, equipment: List[Day11Equipment]) {
  def debug(elevator: Int, equipmentIds: List[String]): String = {
    val fl = s"F$number "
    val el = if (elevator == number) "E  " else ".  "
    s"$fl$el" + equipmentIds.map { id =>
      if (equipment.exists(_.id == id)) s"$id "
      else ".  "
    }.mkString
  }

  val isSafe: Boolean = {
    if (equipment.length == 1) true
    else {
      val ms = equipment.collect { case m: Day11Equipment.Microchip => m }
      val gs = equipment.collect { case g: Day11Equipment.RTG => g }
      val allMs = ms.length == equipment.length
      val matchedMs = ms.forall(m => gs.exists(_.id.startsWith(m.id.slice(0, 1))))
      allMs || matchedMs
    }
  }
}

object Day11Floor {
  private val numberPattern = "The ([a-z]+) floor.*".r

  def from(line: String): Day11Floor = {
    val number = line match {
      case numberPattern("first")  => 1
      case numberPattern("second") => 2
      case numberPattern("third")  => 3
      case numberPattern("fourth") => 4
    }
    line.split("contains a").lift(1) match {
      case None => Day11Floor(number, List.empty)
      case Some(equipmentString) =>
        val equipmentAnd = equipmentString.split(",? and an? ")
        val equipmentDetails =
          (equipmentAnd(0).split(", an? ").map(e => Some(e.trim)) :+ equipmentAnd
            .lift(1)
            .map(_.trim.dropRight(1))).flatten
        //println(equipmentDetails.toList)
        val equipment = equipmentDetails.map(Day11Equipment.from).toList
        Day11Floor(number, equipment)
    }
  }
}

case class Day11ContainmentArea(elevator: Int, floors: Map[Int, Day11Floor]) {
  override def toString: String = {
    val equipmentIds = floors.flatMap(_._2.equipment.map(_.id)).toList.sorted
    floors.keys.toList.sortBy(-_).map(k => floors(k).debug(elevator, equipmentIds)).mkString("\n")
  }

  def updated(floor: Int, equipment: List[Day11Equipment]): Day11ContainmentArea = {
    val newFloors = floors.map { f =>
      if (f._1 == elevator) (f._1, Day11Floor(f._1, f._2.equipment diff equipment))
      else if (f._1 == floor) (f._1, Day11Floor(f._1, f._2.equipment ++ equipment))
      else f
    }

    Day11ContainmentArea(floor, newFloors)
  }
}

case class Day11Data(equipment: Vector[Day11Equipment], state: Day11ContainmentArea)

case class Day11Context(additionalEquipment: Map[Int, List[Day11Equipment]])

object Day11 extends Day[Day11Data, Day11Context, Int](2016, 11) {

  case class Day11EncoderDecoder(equipment: Vector[Day11Equipment]) {
    private val canonicalEquipment = equipment.reverse.zipWithIndex

    def toInt(area: Day11ContainmentArea): Int =
      canonicalEquipment.map { case (e, i) =>
        math.pow(4, i.toDouble).toInt * area.floors.find(_._2.equipment.contains(e)).map(_._1 - 1).getOrElse(0)
      }.sum + math.pow(4, equipment.length.toDouble).toInt * (area.elevator - 1)

    def fromInt(value: Int): Day11ContainmentArea = {
      val indexes =
        BigInt(value).toString(4).reverse.padTo(equipment.length + 1, '0').reverse.toList.map(c => (c - '0') + 1)
      val elevator = indexes.head
      val equipmentByFloor = indexes
        .slice(1, indexes.length)
        .zipWithIndex
        .groupBy(_._1)
        .map(kv => (kv._1, kv._2.map(ei => equipment(ei._2))))

      val floors = (1 to 4).map { number =>
        number -> Day11Floor(number, equipmentByFloor.getOrElse(number, List.empty))
      }.toMap

      Day11ContainmentArea(elevator, floors)
    }
  }

  override def transformInput(lines: List[String]): Day11Data = {
    val floors = lines.map(Day11Floor.from).map(f => (f.number, f)).toMap
    val equipment = floors.flatMap(_._2.equipment).toVector.sortBy(_.id)
    Day11Data(equipment, Day11ContainmentArea(1, floors))
  }

  override def partOneContext(): Option[Day11Context] =
    Some(Day11Context(Map.empty))

  override def partTwoContext(): Option[Day11Context] =
    Some(
      Day11Context(
        Map(
          1 -> List(
            Day11Equipment.RTG("EG"),
            Day11Equipment.Microchip("EM"),
            Day11Equipment.RTG("DG"),
            Day11Equipment.Microchip("DM")
          )
        )
      )
    )

  override def process(input: Day11Data, context: Option[Day11Context]): Option[Int] = context.flatMap { ctx =>
    val updatedEquipment = input.equipment ++ ctx.additionalEquipment.values.flatten
    val updatedArea = ctx.additionalEquipment.foldLeft(input.state) { case (state, (floor, equipment)) =>
      state.updated(floor, equipment)
    }
    val updatedData = Day11Data(updatedEquipment, updatedArea)
    solve(updatedData)
  }

  private def solve(data: Day11Data): Option[Int] = {
    case class WorkItem(steps: Int, data: Int)
    def priorityOrder(w: WorkItem) = -w.steps

    val finish = math.pow(4, data.equipment.length.toDouble + 1).toInt - 1
    val encoderDecoder = Day11EncoderDecoder(data.equipment)
    val queue = scala.collection.mutable
      .PriorityQueue[WorkItem](WorkItem(0, encoderDecoder.toInt(data.state)))(Ordering.by(priorityOrder))

    val visited = Array.fill(finish)(false)
    var stepNum = -1

    while (queue.nonEmpty) {
      val WorkItem(currentSteps, currentData) = queue.dequeue()
      if (currentSteps > stepNum) {
        println(s"step $currentSteps\tqueue: ${queue.size}")
        stepNum = currentSteps
      }
      if (currentData == finish)
        return Some(currentSteps)
      if (!visited(currentData)) {
        visited(currentData) = true
        val area = encoderDecoder.fromInt(currentData)
        val floor = area.floors(area.elevator)
        if (floor.isSafe) {
          val combos = floor.equipment.combinations(1) ++ floor.equipment.combinations(2)
          val newWork = combos
            .flatMap(c => List((area.elevator - 1, c), (area.elevator + 1, c)))
            .filter { case (floor, _) => floor >= 1 && floor <= 4 }
            .flatMap { case (floor, equipment) =>
              val updatedArea = area.updated(floor, equipment)
              if (updatedArea.floors(floor).isSafe)
                Some(updatedArea)
              else None
            }
            .map(area => WorkItem(currentSteps + 1, encoderDecoder.toInt(area)))

          queue.enqueue(newWork.toSeq: _*)
        }
      }
    }

    None
  }
}
