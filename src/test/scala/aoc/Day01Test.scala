package aoc

import org.scalactic.TypeCheckedTripleEquals
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

final class Day01Test extends AnyFlatSpec with Matchers with TypeCheckedTripleEquals:
  val input = Calories.parseInventory("""1000
                                        |2000
                                        |3000
                                        |
                                        |4000
                                        |
                                        |5000
                                        |6000
                                        |
                                        |7000
                                        |8000
                                        |9000
                                        |
                                        |10000
                                        |""".stripMargin.linesIterator)

  "Part 1" `should` "find the top group" in {
    Day01.part1(input) should ===(24000)
  }

  "Part 2" `should` "find top three groups" in {
    Day01.part2(input) should ===(45000)
  }
