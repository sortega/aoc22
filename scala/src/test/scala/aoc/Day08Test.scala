package aoc

import aoc.Day08.*

final class Day08Test extends UnitTest:
  val testInput = Forest.parse(
    Inputs(
      """30373
        |25512
        |65332
        |33549
        |35390
        |""".stripMargin
    )
  )
  val input = Forest.parse(Inputs(day = 8))

  "Part 1" `should` "count trees visible from the exterior" in {
    part1(testInput) should ===(21)
    part1(input) should ===(1533)
  }

  "Part 2" `should` "report the maximum scenic score" in {
    part2(testInput) should ===(8)
    part2(input) should ===(345744)
  }
