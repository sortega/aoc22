package aoc

import aoc.Day14.*

final class Day14Test extends UnitTest:
  val input = Cave.parse(
    Inputs(
      """498,4 -> 498,6 -> 496,6
      |503,4 -> 502,4 -> 502,9 -> 494,9
      |""".stripMargin
    )
  )

  "Input" `should` "be parsed" in {
    input.toString should ===("""....#...##
                                |....#...#.
                                |..###...#.
                                |........#.
                                |........#.
                                |#########.""".stripMargin)
  }

  "Part 1" `should` "count how much sand accumulates" in {
    part1(input) should ===(24)
  }

  "Part 2" `should` "do the same with a floor" in {
    part2(input) should ===(93)
  }
