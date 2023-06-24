package aoc

import aoc.Day06.*

final class Day06Test extends UnitTest:
  "Part 1" `should` "detect the offset of the package start signal" in {
    part1("mjqjpqmgbljsphdztnvjfqwrcgsmlb") should ===(7)
    part1("bvwbjplbgvbhsrlpgdmjqwftvncz") should ===(5)
    part1("nppdvjthqldpwncqszvftbrmjlhg") should ===(6)
  }

  "Part 2" `should` "detect the offset of the package end signal" in {
    part2("mjqjpqmgbljsphdztnvjfqwrcgsmlb") should ===(19)
    part2("bvwbjplbgvbhsrlpgdmjqwftvncz") should ===(23)
    part2("nppdvjthqldpwncqszvftbrmjlhg") should ===(23)
  }
