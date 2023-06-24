package aoc

import cats.*
import cats.data.*
import cats.implicits.*
import aoc.Day10.*

final class Day10Test extends UnitTest:
  val input = Inputs(
    """noop
      |addx 3
      |addx -5
      |""".stripMargin
  ).parseLines(Command.parser)
  val largerInput = Inputs(day = 10, test = true).parseLines(Command.parser)
  val actualInput = Inputs(day = 10).parseLines(Command.parser)

  "Machine" `should` "track register values cycle by cycle" in {
    Machine.Initial.execute(input) should ===(
      Machine(x = -1).writer(Vector(1, 1, 1, 4, 4))
    )
  }

  "Part 1" `should` "count positions visited by the tail" in {
    part1(largerInput) should ===(13140)
    part1(actualInput) should ===(11960)
  }

  "Part 2" `should` "count positions visited by the tail for ten knots" in {
    part2(largerInput) should ===("""##..##..##..##..##..##..##..##..##..##..
                                    |###...###...###...###...###...###...###.
                                    |####....####....####....####....####....
                                    |#####.....#####.....#####.....#####.....
                                    |######......######......######......####
                                    |#######.......#######.......#######.....""".stripMargin)
  }
