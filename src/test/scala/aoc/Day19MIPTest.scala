package aoc

import aoc.Day19MIP.*
import com.google.ortools.Loader

final class Day19MIPTest extends UnitTest:
  Loader.loadNativeLibraries()

  val input = Blueprint.parseAll(
    """Blueprint 1: Each ore robot costs 4 ore. Each clay robot costs 2 ore. Each obsidian robot costs 3 ore and 14 clay. Each geode robot costs 2 ore and 7 obsidian.
      |Blueprint 2: Each ore robot costs 2 ore. Each clay robot costs 3 ore. Each obsidian robot costs 3 ore and 8 clay. Each geode robot costs 3 ore and 12 obsidian.
      |""".stripMargin
  )
  val actualInput = Inputs(day = 19).parseLines(Blueprint.parse)

  "Search" `should` "find the optimal for blueprint 1" in {
    Plan.findBestFor(input.head, timeLimit = 24).score should ===(9)
    Plan.findBestFor(input.head, timeLimit = 32).score should ===(56)
    Plan.findBestFor(input(1), timeLimit = 32).score should ===(62)
  }

  "Part 1" `should` "add up the quality levels" in {
    part1(input) should ===(33)
  }
