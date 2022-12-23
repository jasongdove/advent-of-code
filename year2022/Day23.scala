package adventofcode.year2022

import adventofcode.Day
import cats.effect._

object Day23 extends IOApp {
  case class ElfLocation(row: Int, col: Int)

  def printGrove(elves: List[ElfLocation]) = {
    val rows = elves.map(_.row)
    val cols = elves.map(_.col)
    for (r <- rows.min to rows.max) {
      for (c <- cols.min to cols.max) {
        val location = ElfLocation(r, c)
        if (elves.contains(location)) {
          print('#')
        } else {
          print('.')
        }
      }
      println()
    }
  }

  case class Input(elves: List[ElfLocation])

  case object Input {
    def from(lines: List[String]): Input = {
      val elves = lines.zipWithIndex.flatMap { case (line, row) =>
        line.zipWithIndex.flatMap {
          case ('#', col) => Some(ElfLocation(row, col))
          case _          => None
        }
      }

      Input(elves)
    }
  }

  def solve(inputElves: List[ElfLocation], rounds: Option[Int]): (List[ElfLocation], Int) = {
    case class ElfProposal(deltaChecks: List[ElfLocation], deltaMove: ElfLocation)

    val elves = scala.collection.mutable.Set.from(inputElves)

    val elfProposalSet = List(
      ElfProposal(
        List(ElfLocation(-1, -1), ElfLocation(-1, 0), ElfLocation(-1, 1)),
        ElfLocation(-1, 0)
      ),
      ElfProposal(
        List(ElfLocation(1, -1), ElfLocation(1, 0), ElfLocation(1, 1)),
        ElfLocation(1, 0)
      ),
      ElfProposal(
        List(ElfLocation(-1, -1), ElfLocation(0, -1), ElfLocation(1, -1)),
        ElfLocation(0, -1)
      ),
      ElfProposal(
        List(ElfLocation(-1, 1), ElfLocation(0, 1), ElfLocation(1, 1)),
        ElfLocation(0, 1)
      )
    )

    for (roundIndex <- 0 until rounds.getOrElse(Int.MaxValue)) {
      // find elves that can move
      val elvesToMove = elves.filter { elf =>
        val neighbors = List(
          ElfLocation(elf.row - 1, elf.col - 1),
          ElfLocation(elf.row - 1, elf.col),
          ElfLocation(elf.row - 1, elf.col + 1),
          ElfLocation(elf.row, elf.col - 1),
          ElfLocation(elf.row, elf.col + 1),
          ElfLocation(elf.row + 1, elf.col - 1),
          ElfLocation(elf.row + 1, elf.col),
          ElfLocation(elf.row + 1, elf.col + 1)
        )
        elves.exists(neighbors.contains)
      }

      // propose moves
      val proposals = scala.collection.mutable.Map.empty[ElfLocation, ElfLocation]
      for (elf <- elvesToMove) {
        for (proposal <- (0 to 3).map(i => elfProposalSet((roundIndex + i) % elfProposalSet.size))) {
          if (!proposals.contains(elf)) {
            val checks = proposal.deltaChecks.map(d => ElfLocation(elf.row + d.row, elf.col + d.col))
            if (checks.forall(elves.contains(_) == false)) {
              proposals.addOne(elf, ElfLocation(elf.row + proposal.deltaMove.row, elf.col + proposal.deltaMove.col))
            }
          }
        }
      }

      // filter proposals
      val validProposals = proposals.groupBy(_._2).filter(_._2.size == 1).flatMap(_._2)

      if (validProposals.size == 0) {
        return (elves.toList, roundIndex)
      }

      // perform moves
      for (move <- validProposals) {
        elves.remove(move._1)
        elves.addOne(move._2)
      }
    }

    (elves.toList, 0)
  }

  case class Context(solve: Input => Int)

  object Runner extends Day[Input, Context, Int](2022, 23) {
    override def transformInput(lines: List[String]): Input =
      Input.from(lines)

    override def partOneContext(): Option[Context] =
      Some(Context { input =>
        val (result, _) = solve(input.elves, Some(10))
        val rows = result.map(_.row)
        val cols = result.map(_.col)

        val numRows = rows.max - rows.min + 1
        val numCols = cols.max - cols.min + 1

        numRows * numCols - result.size
      })

    override def partTwoContext(): Option[Context] =
      Some(Context { input =>
        val (_, roundIndex) = solve(input.elves, None)
        roundIndex + 1
      })

    override def process(input: Input, context: Option[Context]): Option[Int] =
      context.map(_.solve(input))
  }

  override def run(args: List[String]): IO[ExitCode] = Runner.run(args)
}
