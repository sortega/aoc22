package aoc

import cats.*
import cats.data.*
import cats.implicits.*
import aoc.Day11.*
import scala.collection.immutable.Queue

final class Day11Test extends UnitTest:
  val input = Inputs(
    """Monkey 0:
      |  Starting items: 79, 98
      |  Operation: new = old * 19
      |  Test: divisible by 23
      |    If true: throw to monkey 2
      |    If false: throw to monkey 3
      |
      |Monkey 1:
      |  Starting items: 54, 65, 75, 74
      |  Operation: new = old + 6
      |  Test: divisible by 19
      |    If true: throw to monkey 2
      |    If false: throw to monkey 0
      |
      |Monkey 2:
      |  Starting items: 79, 60, 97
      |  Operation: new = old * old
      |  Test: divisible by 13
      |    If true: throw to monkey 1
      |    If false: throw to monkey 3
      |
      |Monkey 3:
      |  Starting items: 74
      |  Operation: new = old + 3
      |  Test: divisible by 17
      |    If true: throw to monkey 0
      |    If false: throw to monkey 1
      |
      |""".stripMargin
  ).parse(MonkeyTroop.parser)
  val actualInput = Inputs(day = 11).parse(MonkeyTroop.parser)

  "A monkey troop" `should` "gather stats for a round" in {
    input.nextRound(cooldown = true) should ===(
      MonkeyTroop(
        Map(
          0 -> Monkey(Queue(20, 23, 27, 26), Operation.Multiply(19), Test(23, 2, 3)),
          1 -> Monkey(Queue(2080, 25, 167, 207, 401, 1046), Operation.Add(6), Test(19, 2, 0)),
          2 -> Monkey(Queue.empty, Operation.Square, Test(13, 1, 3)),
          3 -> Monkey(Queue.empty, Operation.Add(3), Test(17, 0, 1)),
        )
      ).writer(
        Map(
          0 -> 2,
          1 -> 4,
          2 -> 3,
          3 -> 5,
        )
      )
    )
  }

  "Part 1" `should` "compute monkey activity after 20 rounds" in {
    part1(input) should ===(10605L)
    part1(actualInput) should ===(64032L)
  }

  "Part 2" `should` "compute monkey activity after 10000 rounds" in {
    part2(input) should ===(2713310158L)
    part2(actualInput) should ===(12729522272L)
  }
