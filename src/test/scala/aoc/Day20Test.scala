package aoc

import aoc.Day20.*

final class Day20Test extends UnitTest:
  val input = List[Long](1, 2, -3, 3, -2, 0, 4)
  val file = toFile(input)

  "A file" `should` "be mixed" in {
    mix(file) should ===(
      Vector(
        Value(offset = 0, 1),
        Value(offset = 1, 2),
        Value(offset = 2, -3),
        Value(offset = 6, 4),
        Value(offset = 5, 0),
        Value(offset = 3, 3),
        Value(offset = 4, -2),
      )
    )
  }

  "Part 1" `should` "decrypt the file" in {
    part1(file) should ===(3L)
  }

  "Part 2" `should` "work" in {
    part2(file) should ===(1623178306L)
  }
