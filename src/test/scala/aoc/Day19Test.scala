package aoc

import aoc.Day19.*

final class Day19Test extends UnitTest:
  val input = Blueprint.parseAll(
    """Blueprint 1: Each ore robot costs 4 ore. Each clay robot costs 2 ore. Each obsidian robot costs 3 ore and 14 clay. Each geode robot costs 2 ore and 7 obsidian.
      |Blueprint 2: Each ore robot costs 2 ore. Each clay robot costs 3 ore. Each obsidian robot costs 3 ore and 8 clay. Each geode robot costs 3 ore and 12 obsidian.
      |""".stripMargin
  )

  val actualInput = Inputs(day = 19).parseLines(Blueprint.parse)

  "Search" `should` "work" in {
    mostGeodes(minutes = 24, input.head) should ===(9)
    mostGeodes(minutes = 32, input.head) should ===(56)
    mostGeodes(minutes = 32, input(2)) should ===(62)
  }

  "Part 1" `should` "add up the quality levels" in {
    part1(input) should ===(33)
  }

  "Part 2" `should` "work" in {
    part2(input) should ===(62)
  }
