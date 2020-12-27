package adventofcode.year2016

import adventofcode.Day
import cats.effect._

object Day11 extends IOApp {
  sealed trait Equipment {
    def id: String
  }

  object Equipment {
    case class Microchip(id: String) extends Equipment

    case class RTG(id: String) extends Equipment

    def from(str: String): Equipment = {
      if (str.contains("microchip")) Microchip(s"${str.head.toUpper}M")
      else RTG(s"${str.head.toUpper}G")
    }
  }

  case class Floor(number: Int, equipment: List[Equipment]) {
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
        val ms = equipment.collect { case m: Equipment.Microchip => m }
        val gs = equipment.collect { case g: Equipment.RTG => g }
        val allMs = ms.length == equipment.length
        val matchedMs = ms.forall(m => gs.exists(_.id.startsWith(m.id.slice(0, 1))))
        allMs || matchedMs
      }
    }
  }

  object Floor {
    private val numberPattern = "The ([a-z]+) floor.*".r

    def from(line: String): Floor = {
      val number = line match {
        case numberPattern("first")  => 1
        case numberPattern("second") => 2
        case numberPattern("third")  => 3
        case numberPattern("fourth") => 4
      }
      line.split("contains a").lift(1) match {
        case None => Floor(number, List.empty)
        case Some(equipmentString) =>
          val equipmentAnd = equipmentString.split(",? and an? ")
          val equipmentDetails =
            (equipmentAnd(0).split(", an? ").map(e => Some(e.trim)) :+ equipmentAnd
              .lift(1)
              .map(_.trim.dropRight(1))).flatten
          //println(equipmentDetails.toList)
          val equipment = equipmentDetails.map(Equipment.from).toList
          Floor(number, equipment)
      }
    }
  }

  case class ContainmentArea(elevator: Int, floors: Map[Int, Floor]) {
    override def toString: String = {
      val equipmentIds = floors.flatMap(_._2.equipment.map(_.id)).toList.sorted
      floors.keys.toList.sortBy(-_).map(k => floors(k).debug(elevator, equipmentIds)).mkString("\n")
    }

    def updated(floor: Int, equipment: List[Equipment]): ContainmentArea = {
      val newFloors = floors.map { f =>
        if (f._1 == elevator) (f._1, Floor(f._1, f._2.equipment diff equipment))
        else if (f._1 == floor) (f._1, Floor(f._1, f._2.equipment ++ equipment))
        else f
      }

      ContainmentArea(floor, newFloors)
    }
  }

  case class Data(equipment: Vector[Equipment], state: ContainmentArea)

  case class Context(additionalEquipment: Map[Int, List[Equipment]])

  case class EncoderDecoder(equipment: Vector[Equipment]) {
    private val canonicalEquipment = equipment.reverse.zipWithIndex

    def toInt(area: ContainmentArea): Int =
      canonicalEquipment.map { case (e, i) =>
        math.pow(4, i.toDouble).toInt * area.floors.find(_._2.equipment.contains(e)).map(_._1 - 1).getOrElse(0)
      }.sum + math.pow(4, equipment.length.toDouble).toInt * (area.elevator - 1)

    def fromInt(value: Int): ContainmentArea = {
      val indexes =
        BigInt(value).toString(4).reverse.padTo(equipment.length + 1, '0').reverse.toList.map(c => (c - '0') + 1)
      val elevator = indexes.head
      val equipmentByFloor = indexes
        .slice(1, indexes.length)
        .zipWithIndex
        .groupBy(_._1)
        .map(kv => (kv._1, kv._2.map(ei => equipment(ei._2))))

      val floors = (1 to 4).map { number =>
        number -> Floor(number, equipmentByFloor.getOrElse(number, List.empty))
      }.toMap

      ContainmentArea(elevator, floors)
    }
  }

  private def solve(data: Data): Option[Int] = {
    case class WorkItem(steps: Int, data: Int)
    def priorityOrder(w: WorkItem) = -w.steps

    val finish = math.pow(4, data.equipment.length.toDouble + 1).toInt - 1
    val encoderDecoder = EncoderDecoder(data.equipment)
    val queue = scala.collection.mutable
      .PriorityQueue[WorkItem](WorkItem(0, encoderDecoder.toInt(data.state)))(Ordering.by(priorityOrder))

    val visited = Array.fill(finish)(false)
    var stepNum = -1

    while (queue.nonEmpty) {
      val WorkItem(currentSteps, currentData) = queue.dequeue()
      if (currentSteps > stepNum) {
        // println(s"step $currentSteps\tqueue: ${queue.size}")
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

  object Runner extends Day[Data, Context, Int](2016, 11) {
    override def transformInput(lines: List[String]): Data = {
      val floors = lines.map(Floor.from).map(f => (f.number, f)).toMap
      val equipment = floors.flatMap(_._2.equipment).toVector.sortBy(_.id)
      Data(equipment, ContainmentArea(1, floors))
    }

    override def partOneContext(): Option[Context] =
      Some(Context(Map.empty))

    override def partTwoContext(): Option[Context] =
      Some(
        Context(
          Map(
            1 -> List(
              Equipment.RTG("EG"),
              Equipment.Microchip("EM"),
              Equipment.RTG("DG"),
              Equipment.Microchip("DM")
            )
          )
        )
      )

    override def process(input: Data, context: Option[Context]): Option[Int] = context.flatMap { ctx =>
      val updatedEquipment = input.equipment ++ ctx.additionalEquipment.values.flatten
      val updatedArea = ctx.additionalEquipment.foldLeft(input.state) { case (state, (floor, equipment)) =>
        state.updated(floor, equipment)
      }
      val updatedData = Data(updatedEquipment, updatedArea)
      solve(updatedData)
    }
  }

  override def run(args: List[String]): IO[ExitCode] = Runner.run(args)
}
