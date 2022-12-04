package aoc

import aoc.Day04.*

final class Day04Test extends UnitTest:
  val input = Input(
    """2-4,6-8
      |2-3,4-5
      |5-7,7-9
      |2-8,3-7
      |6-6,4-6
      |2-6,4-8
      |""".stripMargin).parseLines(Assignment.parse)

  "Part 1" `should` "count assignment pairs with full overlap" in {
    part1(input) should ===(2)
  }

  "Part 2" `should` "count assignment paris that overlap at all" in {
    part2(input) should ===(4)
  }
