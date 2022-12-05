package aoc

import atto.*, Atto.*
import cats.implicits.*

object Day04:
  type Input = List[Assignment]

  case class Assignment(left: Range.Inclusive, right: Range.Inclusive):
    def hasFullOverlap: Boolean = left.containsRange(right) || right.containsRange(left)
    def hasOverlap: Boolean = left.intersect(right).nonEmpty

  object Assignment:
    val rangeParser: Parser[Range.Inclusive] = (int <~ char('-'), int).mapN(Range.inclusive)
    val parser: Parser[Assignment] = (rangeParser <~ char(','), rangeParser).mapN(Assignment.apply)

  extension (interval: Range.Inclusive)
    def containsRange(other: Range.Inclusive): Boolean =
      interval.contains(other.start) && interval.contains(other.end)

  def part1(input: Input): Int = input.count(_.hasFullOverlap)

  def part2(input: Input): Int = input.count(_.hasOverlap)

  def main(args: Array[String]): Unit =
    val input = Inputs(day = 4).parseLines(Assignment.parser)
    println(part1(input))
    println(part2(input))
