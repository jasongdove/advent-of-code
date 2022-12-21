package adventofcode.year2022

import adventofcode.Day
import cats.effect._

object Day21 extends IOApp {
  sealed trait Operation

  object Operation {
    case object Add extends Operation
    case object Sub extends Operation
    case object Mul extends Operation
    case object Div extends Operation
  }

  sealed trait Job

  case class YellNumber(number: Long) extends Job
  case class WaitToYell(monkeyOne: String, monkeyTwo: String, operation: Operation) extends Job

  case class Monkey(name: String, job: Job)

  case object Monkey {
    private val yellPattern = "([a-z]{4}): (\\d+)".r
    private val waitAddPattern = "([a-z]{4}): ([a-z]{4}) \\+ ([a-z]{4})".r
    private val waitSubPattern = "([a-z]{4}): ([a-z]{4}) \\- ([a-z]{4})".r
    private val waitMulPattern = "([a-z]{4}): ([a-z]{4}) \\* ([a-z]{4})".r
    private val waitDivPattern = "([a-z]{4}): ([a-z]{4}) \\/ ([a-z]{4})".r

    def from(line: String): Monkey =
      line match {
        case yellPattern(name, number) =>
          Monkey(name, YellNumber(number.toInt))
        case waitAddPattern(name, one, two) =>
          Monkey(name, WaitToYell(one, two, Operation.Add))
        case waitSubPattern(name, one, two) =>
          Monkey(name, WaitToYell(one, two, Operation.Sub))
        case waitMulPattern(name, one, two) =>
          Monkey(name, WaitToYell(one, two, Operation.Mul))
        case waitDivPattern(name, one, two) =>
          Monkey(name, WaitToYell(one, two, Operation.Div))
      }
  }

  case class Input(monkeys: List[Monkey])

  case object Input {
    def from(lines: List[String]): Input = {
      Input(lines.map(Monkey.from))
    }
  }

  case class State(
    waitingMonkeys: scala.collection.mutable.Set[Monkey],
    completedMonkeys: scala.collection.mutable.HashMap[String, Double]
  )

  def solve(monkeys: List[Monkey]): State =
    solve(scala.collection.mutable.Set.from(monkeys), scala.collection.mutable.HashMap.empty[String, Double])

  def solve(
    waitingMonkeys: scala.collection.mutable.Set[Monkey],
    completedMonkeys: scala.collection.mutable.HashMap[String, Double]
  ): State = {
    val toRemove = scala.collection.mutable.ArrayBuffer.empty[Monkey]
    do {
      toRemove.clear()
      for (m <- waitingMonkeys) {
        m.job match {
          case YellNumber(number) =>
            completedMonkeys.addOne(m.name, number)
            toRemove.addOne(m)
          case WaitToYell(monkeyOne, monkeyTwo, operation)
              if (completedMonkeys.contains(monkeyOne) && completedMonkeys.contains(monkeyTwo)) =>
            val one = completedMonkeys(monkeyOne)
            val two = completedMonkeys(monkeyTwo)
            val number = operation match {
              case Operation.Add => one + two
              case Operation.Div => one / two
              case Operation.Mul => one * two
              case Operation.Sub => one - two
            }
            completedMonkeys.addOne(m.name, number)
            toRemove.addOne(m)
          case _ => ()
        }
      }
      for (r <- toRemove) {
        waitingMonkeys.remove(r)
      }
    } while (toRemove.size > 0)

    State(waitingMonkeys, completedMonkeys)
  }

  def solveTwo(monkeys: List[Monkey]): Double = {
    val rootJob = monkeys.find(_.name == "root").get.job.asInstanceOf[WaitToYell]
    val reduced = solve(monkeys.filterNot(m => m.name == "humn" || m.name == "root"))

    val knownMonkey = rootJob.monkeyTwo
    var result = reduced.completedMonkeys(knownMonkey)

    var monkeyToFind = rootJob.monkeyOne
    while (monkeyToFind != "humn") {
      reduced.waitingMonkeys.find(_.name == monkeyToFind).get.job match {
        case WaitToYell(one, two, operation) if reduced.completedMonkeys.contains(one) =>
          val other = reduced.completedMonkeys(one)
          result = operation match {
            case Operation.Add => result - other
            case Operation.Sub => other - result
            case Operation.Div => result * other
            case Operation.Mul => result / other
          }
          monkeyToFind = two
        case WaitToYell(one, two, operation) if reduced.completedMonkeys.contains(two) =>
          val other = reduced.completedMonkeys(two)
          result = operation match {
            case Operation.Add => result - other
            case Operation.Sub => result + other
            case Operation.Div => result * other
            case Operation.Mul => result / other
          }
          monkeyToFind = one
        case _ => ()
      }
    }

    result
  }

  case class Context(solve: Input => Long)

  object Runner extends Day[Input, Context, Long](2022, 21) {
    override def transformInput(lines: List[String]): Input =
      Input.from(lines)

    override def partOneContext(): Option[Context] =
      Some(Context(input => {
        solve(input.monkeys).completedMonkeys("root").toLong
      }))

    override def partTwoContext(): Option[Context] =
      Some(Context(input => {
        solveTwo(input.monkeys).toLong
      }))

    override def process(input: Input, context: Option[Context]): Option[Long] =
      context.map(_.solve(input))
  }

  override def run(args: List[String]): IO[ExitCode] = Runner.run(args)
}
