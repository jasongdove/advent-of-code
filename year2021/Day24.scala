// using java-opt -Xms128m
// using java-opt -Xmx4g
// using lib it.unimi.dsi:fastutil:8.5.6

package adventofcode.year2021

import cats.effect._
import adventofcode._

object Day24 extends IOApp {
  case class Subprogram(divZ: Int, addX: Int, addY: Int) {
    def run(z: Int, w: Int): Int = {
      val x = if ((z % 26) + addX != w) 1 else 0
      val y = (25 * x) + 1
      ((z / divZ) * y) + ((w + addY) * x)
    }
  }

  object Subprogram {
    import AsmInstruction._

    def from(instructions: Seq[AsmInstruction]): Subprogram = {
      val divZ = instructions.collectFirst { case Div('z', o) => o }.get
      val addX = instructions.collectFirst { case Add('x', o) => o }.get
      val addY = instructions.reverse.collectFirst { case Add('y', o) => o }.get
      Subprogram(divZ, addX, addY)
    }
  }

  sealed trait AsmInstruction

  // TODO: could probably clean this up since the eval loop was removed
  object AsmInstruction {
    case class Inp(dest: Char) extends AsmInstruction
    case class Add(dest: Char, operand: Int) extends AsmInstruction
    case class AddReg(dest: Char, operand: Char) extends AsmInstruction
    case class Mul(dest: Char, operand: Int) extends AsmInstruction
    case class MulReg(dest: Char, operand: Char) extends AsmInstruction
    case class Div(dest: Char, operand: Int) extends AsmInstruction
    case class DivReg(dest: Char, operand: Char) extends AsmInstruction
    case class Mod(dest: Char, operand: Int) extends AsmInstruction
    case class ModReg(dest: Char, operand: Char) extends AsmInstruction
    case class Eql(dest: Char, operand: Int) extends AsmInstruction
    case class EqlReg(dest: Char, operand: Char) extends AsmInstruction
    case class Noop() extends AsmInstruction

    private val inputPattern = "inp ([wxyz])".r
    private val addPattern = "add ([wxyz]) (-?\\d+)".r
    private val addRegPattern = "add ([wxyz]) ([wxyz])".r
    private val multiplyPattern = "mul ([wxyz]) (-?\\d+)".r
    private val multiplyRegPattern = "mul ([wxyz]) ([wxyz])".r
    private val dividePattern = "div ([wxyz]) (-?\\d+)".r
    private val divideRegPattern = "div ([wxyz]) ([wxyz])".r
    private val modPattern = "mod ([wxyz]) (-?\\d+)".r
    private val modRegPattern = "mod ([wxyz]) ([wxyz])".r
    private val eqlPattern = "eql ([wxyz]) (-?\\d+)".r
    private val eqlRegPattern = "eql ([wxyz]) ([wxyz])".r

    def from(line: String): AsmInstruction =
      line match {
        case inputPattern(dest)                => Inp(dest.head)
        case addPattern(dest, operand)         => Add(dest.head, operand.toInt)
        case addRegPattern(dest, operand)      => AddReg(dest.head, operand.head)
        case multiplyPattern(dest, operand)    => Mul(dest.head, operand.toInt)
        case multiplyRegPattern(dest, operand) => MulReg(dest.head, operand.head)
        case dividePattern(dest, operand)      => Div(dest.head, operand.toInt)
        case divideRegPattern(dest, operand)   => DivReg(dest.head, operand.head)
        case modPattern(dest, operand)         => Mod(dest.head, operand.toInt)
        case modRegPattern(dest, operand)      => ModReg(dest.head, operand.head)
        case eqlPattern(dest, operand)         => Eql(dest.head, operand.toInt)
        case eqlRegPattern(dest, operand)      => EqlReg(dest.head, operand.head)
        case _                                 => Noop()
      }
  }

  case class Input(programs: List[Program])

  case class ProgramInput(z: Int, w: Int, nextZ: Int)

  case class Program(instructions: Seq[AsmInstruction])

  case object Input {
    def from(input: List[String]): Input = {
      val programs = scala.collection.mutable.ArrayBuffer.empty[Program]
      val buf = scala.collection.mutable.ArrayBuffer.empty[AsmInstruction]
      input.foreach { line =>
        val instruction = AsmInstruction.from(line)
        instruction match {
          case AsmInstruction.Inp(_) =>
            if (buf.length > 0) {
              programs.addOne(Program(buf.toSeq))
              buf.clear()
            }
          case _ => ()
        }
        buf.addOne(instruction)
      }
      programs.addOne(Program(buf.toSeq))
      Input(programs.toList)
    }
  }

  case class Visited(queueLen: Int, z: Int)

  case class InputHistory(subQueue: Vector[Subprogram], history: Vector[ProgramInput]) {
    def toVisited = Visited(subQueue.size, history.head.nextZ)
    val num: Long = history.map(_.w).reverse.mkString.toLong
  }

  case class Context(
    priority: InputHistory => Long,
    defaultValue: Long,
    range: Seq[Int],
    alreadyVisited: (Long, Long) => Boolean
  )

  object Runner extends Day[Input, Context, Long](2021, 24) {
    override def transformInput(lines: List[String]): Input =
      Input.from(lines)

    override def partOneContext(): Option[Context] =
      Some(
        Context(
          ih => ih.history.size * 1_000_000_000_000_000L + ih.num,
          0L,
          (9 to 1 by -1),
          (a, b) => a <= b
        )
      )

    override def partTwoContext(): Option[Context] =
      Some(
        Context(
          ih => ih.history.size * 1_000_000_000_000_000L - ih.num,
          Long.MaxValue,
          (1 to 9),
          (a, b) => a > b
        )
      )
    override def process(input: Input, context: Option[Context]): Option[Long] = context.map { ctx =>
      val subs = input.programs.map(p => Subprogram.from(p.instructions))

      val queue = scala.collection.mutable.PriorityQueue.empty[InputHistory](Ordering.by(ctx.priority))
      val visited = new it.unimi.dsi.fastutil.objects.Object2LongOpenHashMap[Visited]
      visited.defaultReturnValue(ctx.defaultValue)
      val done = scala.collection.mutable.ArrayBuffer.empty[InputHistory]

      val subQueue = scala.collection.mutable.Queue.from(subs)
      val first = ctx.range.map { i =>
        val z = subQueue.head.run(0, i)
        InputHistory(subQueue.tail.toVector, Vector(ProgramInput(0, i, z)))
      }

      queue.addAll(first)

      while (!queue.isEmpty && done.size == 0) {
        val head = queue.dequeue()
        if (head.subQueue.isEmpty) {
          if (head.history.head.nextZ == 0)
            done.addOne(head)
        } else {
          val headVisited = head.toVisited
          val headNum = head.num
          if (ctx.alreadyVisited(visited.getLong(headVisited), headNum)) {
            val nextHistory = ctx.range.map { i =>
              val z = head.subQueue.head.run(head.history.head.nextZ, i)
              val nextI = ProgramInput(head.history.head.nextZ, i, z)
              head.copy(
                subQueue = head.subQueue.tail,
                history = nextI +: head.history
              )
            }
            queue.addAll(nextHistory)
            visited.addTo(headVisited, headNum)
          }
        }
      }

      done.head.num
    }
  }

  override def run(args: List[String]): IO[ExitCode] = Runner.run(args)
}
