package aoc

import aoc.CardinalPoint.*
import aoc.Day24.*

final class Day24Test extends UnitTest:
  val input = Valley.parse(
    """#.#####
      |#.....#
      |#>....#
      |#.....#
      |#...v.#
      |#.....#
      |#####.#
      |""".stripMargin
  )
  val complexValley = Valley.parse(
    """#.######
      |#>>.<^<#
      |#.<..<<#
      |#>v.><>#
      |#<^v^^>#
      |######.#
      |""".stripMargin
  )

  "A valley" `should` "be parsed" in {
    input should ===(
      Valley(
        width = 5,
        height = 5,
        blizzards = Set(Blizzard(Pos(2, 1), East), Blizzard(Pos(4, 4), South)),
      )
    )
  }

  it `should` "compute steps" in {
    input.step should ===(
      Valley(
        width = 5,
        height = 5,
        blizzards = Set(Blizzard(Pos(2, 2), East), Blizzard(Pos(5, 4), South)),
      )
    )
    input.step.step should ===(
      Valley(
        width = 5,
        height = 5,
        blizzards = Set(Blizzard(Pos(2, 3), East), Blizzard(Pos(1, 4), South)),
      )
    )
    input.step.step.step.step.step === input
  }

  "Math" `should` "work" in {
    Math.gcd(100, 35) should ===(5)
    Math.lcm(100, 35) should ===(700)
  }

  "Part 1" `should` "find the fastest route" in {
    part1(complexValley) should ===(18)
  }

  "Part 2" `should` "find the fastest route" in {
    part2(complexValley) should ===(54)
  }
