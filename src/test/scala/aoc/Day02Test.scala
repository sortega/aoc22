package aoc

import aoc.Day02.*

final class Day02Test extends UnitTest:
  val input = Guide.parse("""A Y
                            |B X
                            |C Z""".stripMargin.linesIterator)

  "Part 1" `should` "interpret second value as my strategy" in {
    part1(input) should ===(15)
  }

  "Part 2" `should` "interpret second value as the round outcome" in {
    part2(input) should ===(12)
  }
