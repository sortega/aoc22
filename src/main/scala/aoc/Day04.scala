package aoc

object Day04:
  type Input = List[Assignment]

  case class Assignment(left: Range.Inclusive, right: Range.Inclusive):
    def hasFullOverlap: Boolean = left.containsRange(right) || right.containsRange(left)
    def hasOverlap: Boolean = left.intersect(right).nonEmpty

  object Assignment:
    private val Pattern = raw"(\d+)-(\d+),(\d+)-(\d+)".r
    def parse(string: String): Assignment = string match
      case Pattern(start1, end1, start2, end2) =>
        Assignment(start1.toInt to end1.toInt, start2.toInt to end2.toInt)
      case _ => ???

  extension (interval: Range.Inclusive)
    def containsRange(other: Range.Inclusive): Boolean =
      interval.contains(other.start) && interval.contains(other.end)

  def part1(input: Input): Int = input.count(_.hasFullOverlap)

  def part2(input: Input): Int = input.count(_.hasOverlap)

  def main(args: Array[String]): Unit =
    val input = Input(day = 04).parseLines(Assignment.parse)
    println(part1(input))
    println(part2(input))
