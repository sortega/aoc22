package aoc

import aoc.Day03.*

final class Day03Test extends UnitTest:
  val input = Input(
    """vJrwpWtwJgWrhcsFMMfFFhFp
      |jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
      |PmmdzqPrVvPwwTWBwg
      |wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
      |ttgJtRGJQctTZtZT
      |CrZsJsPPZsGzwwsLwLmpwMDw
      |""".stripMargin
  ).parseLines(Rucksack.parse)

  "Items" `should` "have priorities" in {
    priorityOf('a') should ===(1)
    priorityOf('z') should ===(26)
    priorityOf('A') should ===(27)
    priorityOf('Z') should ===(52)
  }

  "Part 1" `should` "sum item priorities" in {
    part1(input) should ===(157)
  }

  "Part 2" `should` "sum group badge priorities" in {
    part2(input) should ===(70)
  }
