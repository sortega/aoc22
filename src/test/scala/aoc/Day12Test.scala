package aoc

import aoc.Day12.*
import cats.*
import cats.data.*
import cats.implicits.*

final class Day12Test extends UnitTest:
  val input = Input.parse(
    """Sabqponm
      |abcryxxl
      |accszExk
      |acctuvwj
      |abdefghi
      |""".stripMargin
  )
  val actualInput = Input.parse(Inputs(day = 12).slurp)

  "Part 1" `should` "find the length of the shortest path" in {
    part1(input) should ===(31)
    part1(actualInput) should ===(352)
  }

  "Part 2" `should` "find the shortest path from any of the 'a' spots" in {
    part2(input) should ===(29)
    part2(actualInput) should ===(345)
  }
