package aoc

import aoc.Day13.*
import cats.*
import cats.data.*
import cats.implicits.*

final class Day13Test extends UnitTest:
  val input = Inputs("""[1,1,3,1,1]
                        |[1,1,5,1,1]
                        |
                        |[[1],[2,3,4]]
                        |[[1],4]
                        |
                        |[9]
                        |[[8,7,6]]
                        |
                        |[[4,4],4,4]
                        |[[4,4],4,4,4]
                        |
                        |[7,7,7,7]
                        |[7,7,7]
                        |
                        |[]
                        |[3]
                        |
                        |[[[]]]
                        |[[]]
                        |
                        |[1,[2,[3,[4,[5,6,7]]]],8,9]
                        |[1,[2,[3,[4,[5,6,0]]]],8,9]
                        |""".stripMargin).parse(Input.parser)
  val actualInput = Inputs(day = 13).parse(Input.parser)

  "Part 1" `should` "sum the indices of the pairs in the right order" in {
    part1(input) should ===(13)
    part1(actualInput) should ===(5720)
  }

  "Part 2" `should` "multiply the indexes of the divider packages" in {
    part2(input) should ===(140)
    part2(actualInput) should ===(23504)
  }
