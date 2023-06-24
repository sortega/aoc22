package aoc

import aoc.Day09.*

final class Day09Test extends UnitTest:
  val input = Inputs(
    """R 4
      |U 4
      |L 3
      |D 1
      |R 4
      |D 1
      |L 5
      |R 2
      |""".stripMargin
  ).parseLines(Command.parse)

  val largerInput = Inputs(
    """R 5
      |U 8
      |L 8
      |D 3
      |R 17
      |D 10
      |L 25
      |U 20
      |""".stripMargin
  ).parseLines(Command.parse)

  val actualInput = Inputs(day = 9).parseLines(Command.parse)

  "State" `should` "have the tail tracking the head" in {
    State(head = Pos(0, 0), tail = List(Pos(0, 0)))
      .moveHead(CardinalPoint.East) `should` ===(State(head = Pos(0, 1), tail = List(Pos(0, 0))))
    State(head = Pos(1, 1), tail = List(Pos(0, 0)))
      .moveHead(CardinalPoint.East) `should` ===(State(head = Pos(1, 2), tail = List(Pos(1, 1))))
  }

  "Part 1" `should` "count positions visited by the tail for two knots" in {
    part1(input) should ===(13)
    part1(actualInput) should ===(6384)
  }

  "Part 2" `should` "count positions visited by the tail for ten knots" in {
    part2(input) should ===(1)
    part2(largerInput) should ===(36)
    part2(actualInput) should ===(2734)
  }
