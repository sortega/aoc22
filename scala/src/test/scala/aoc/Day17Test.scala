package aoc

import aoc.Day17.*

final class Day17Test extends UnitTest:
  private val input = JestPattern.parse(">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>")
  private val actualInput = JestPattern.parse(Inputs(day = 17).slurp)

  "A cave" `should` "have rocks dropped" in {
    val cave = Cave(jestPattern = input)

    val after1 = cave.dropRock
    after1.toString should ===(
      """Time=4 Height=1 Jest=4 Rock=1
        |..####.
        |#######""".stripMargin
    )

    val after2 = after1.dropRock
    after2.toString should ===(
      """Time=8 Height=4 Jest=8 Rock=2
        |...#...
        |..###..
        |...#...
        |..####.
        |#######""".stripMargin
    )

    val after3 = after2.dropRock
    after3.toString should ===(
      """Time=13 Height=6 Jest=13 Rock=3
        |..#....
        |..#....
        |####...
        |..###..
        |...#...
        |..####.
        |#######""".stripMargin
    )
  }

  "Part 1" `should` "compute the height after 2022 iterations" in {
    part1(input) should ===(3068L)
    part1(actualInput) should ===(3191L)
  }

  "Part 2" `should` "compute the height after 1000000000000 iterations" in {
    // WTF: too slow for the small input
    // part2(input) should ===(1514285714288L)
    part2(actualInput) should ===(1572093023267L)
  }
