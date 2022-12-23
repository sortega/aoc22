package aoc

import aoc.CardinalPoint.*
import aoc.Day23.*

final class Day23Test extends UnitTest:
  val input = GameOfElfs.parse(
    """....#..
      |..###.#
      |#...#.#
      |.#...##
      |#.###..
      |##.#.##
      |.#..#..
      |""".stripMargin
  )

  "Rules" `should` "rotate with each turn" in {
    Rule.rulesAt(0).head.move should ===(North)
    Rule.rulesAt(1).head.move should ===(South)
    Rule.rulesAt(6).head.move should ===(West)
    Rule.rulesAt(31).head.move should ===(East)
  }

  "Game of Elfs" `should` "proceed iteration by iteration" in {
    val games = List.iterate(
      GameOfElfs.parse(""".....
                         |..##.
                         |..#..
                         |.....
                         |..##.
                         |.....
                         |""".stripMargin),
      len = 4,
    )(_.step)

    games(1) should ===(
      GameOfElfs
        .parse(
          """..##.
            |.....
            |..#..
            |...#.
            |..#..
            |.....
            |""".stripMargin
        )
        .copy(time = 1)
    )

    games(2) should ===(
      GameOfElfs
        .parse(
          """.....
            |..##.
            |.#...
            |....#
            |.....
            |..#..
            |""".stripMargin
        )
        .copy(time = 2)
    )

    games(3) should ===(
      GameOfElfs
        .parse(
          """..#..
            |....#
            |#....
            |....#
            |.....
            |..#..
            |""".stripMargin
        )
        .copy(time = 3)
    )
  }

  "Part 1" `should` "find empty space in bounding box at iteration 10" in {
    part1(input) should ===(110)
  }

  "Part 2" `should` "work" in {
    part2(input) should ===(20)
  }
