package adventofcode.year2022

import adventofcode.Day
import cats.effect._

object Day5 extends IOApp {

  case class Problem(stacks: List[CrateStack], instructions: List[Instruction])

  case class Instruction(quantity: Int, from: Int, to: Int)

  case class CrateStack(num: Int, stack: List[Char]) {
    def add(c: Char): CrateStack = CrateStack(num, stack :+ c)
    def remove(): CrateStack = CrateStack(num, stack.take(stack.size - 1))
    
    def add(c: List[Char]): CrateStack = CrateStack(num, stack ++ c)
    def remove(qty: Int): CrateStack = CrateStack(num, stack.take(stack.size - qty))
  }

  case class Context(solve: Problem => String)

  object Runner extends Day[Problem, Context, String](2022, 5) {
    override def transformInput(lines: List[String]): Problem = {
      val blank = lines.indexOf("")
      val crateLines = lines.take(blank).reverse
      val maxNum = crateLines.head.flatMap(_.toString.toIntOption).max
      val crateStacks = scala.collection.mutable.ArrayBuffer.empty[CrateStack]
      for (i <- 1 to maxNum) {
        crateStacks.addOne(CrateStack(i, List.empty))
      }
      for (i <- 1 to ((maxNum - 1) * 4 + 1) by 4) {
        val num = crateLines.head(i).toString.toInt
        // println(num)
        for (line <- crateLines.tail) {
          if (line.length() > i && line(i) != ' ') {
            // println(s"updating ${num - 1} with position ${i}")
            crateStacks.update(num - 1, crateStacks(num - 1).add(line(i)))
          }
        }
      }

      val problemLines = lines.takeRight(lines.length - blank - 1)
      val instructions = problemLines.map(line => {
        val split1 = line.split(" from ")
        val quantity = split1(0).split(' ').last.toInt
        // println(quantity)
        val split2 = split1(1).split(" to ")
        val from = split2(0).toInt
        val to = split2(1).toInt
        Instruction(quantity, from, to)
      })
      // println(problemLines)

      Problem(crateStacks.toList, instructions)
    }

    override def partOneContext(): Option[Context] =
      Some(Context(problem => {
        val stacks = scala.collection.mutable.ArrayBuffer.from(problem.stacks)
        for (instruction <- problem.instructions) {
          for (i <- 1 to instruction.quantity) {
            val from = stacks(instruction.from - 1)
            val to = stacks(instruction.to - 1)
            stacks.update(instruction.to - 1, to.add(from.stack.last))
            stacks.update(instruction.from - 1, from.remove())
          }
        }
        stacks.map(_.stack.last).mkString
      }))

    override def partTwoContext(): Option[Context] =
      Some(Context(problem => {
        // println(problem)
        val stacks = scala.collection.mutable.ArrayBuffer.from(problem.stacks)
        for (instruction <- problem.instructions) {
          // println(instruction)
          val from = stacks(instruction.from - 1)
          val to = stacks(instruction.to - 1)
          stacks.update(instruction.to - 1, to.add(from.stack.takeRight(instruction.quantity)))
          stacks.update(instruction.from - 1, from.remove(instruction.quantity))
          // println(stacks)
        }
        stacks.map(_.stack.last).mkString
      }))

    override def process(input: Problem, context: Option[Context]): Option[String] = {
      context.map(_.solve(input))
    }
  }

  override def run(args: List[String]): IO[ExitCode] = Runner.run(args)
}
