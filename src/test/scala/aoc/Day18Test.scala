package aoc

import aoc.Day18.*

final class Day18Test extends UnitTest:
  val input = Inputs(
    """2,2,2
      |1,2,2
      |3,2,2
      |2,1,2
      |2,3,2
      |2,2,1
      |2,2,3
      |2,2,4
      |2,2,6
      |1,2,5
      |3,2,5
      |2,1,5
      |2,3,5
      |""".stripMargin
  ).parseLines(Point.parse).toSet

  val actualInput = Inputs(day = 18).parseLines(Point.parse).toSet

  "Droplets" `should` "be compared layer by layer" ignore {
    tomography(actualInput) should ===(tomography(fill(actualInput)))
  }

  "Part 1" `should` "count unconnected faces" in {
    part1(Set(Point(1, 1, 1), Point(2, 1, 1))) should ===(10)

    part1(input) should ===(64)
    part1(actualInput) should ===(3390)
  }

  "Part 2" `should` "exclude air pockets" in {
    part2(Set(Point(1, 1, 1), Point(2, 1, 1))) should ===(10)
    part2(input) should ===(58)
    part2(actualInput) should ===(2058)
  }
