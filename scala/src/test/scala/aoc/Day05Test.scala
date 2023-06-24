package aoc

import aoc.Day05.*
import atto.*, Atto.*

final class Day05Test extends UnitTest:
  val input = Inputs(
    """    [D]
      |[N] [C]
      |[Z] [M] [P]
      | 1   2   3
      |
      |move 1 from 2 to 1
      |move 3 from 1 to 3
      |move 2 from 2 to 1
      |move 1 from 1 to 2
      |""".stripMargin
  ).parse(Input.parser)

  "Stacks" `should` "be parsed" in {
    Stacks
      .parser
      .parseOnly(
        """[D]
          |[N] [C]
          |[Z] [M] [P]
          | 1   2   3
          |""".stripMargin
      ) should ===(
      ParseResult.Done(
        "",
        Stacks(
          Map(
            1 -> List('D', 'N', 'Z'),
            2 -> List('C', 'M'),
            3 -> List('P'),
          )
        ),
      )
    )
  }

  "Part 1" `should` "move cranes one by one" in {
    part1(input) should ===("CMZ")
  }

  "Part 2" `should` "move cranes in batches" in {
    part2(input) should ===("MCD")
  }
