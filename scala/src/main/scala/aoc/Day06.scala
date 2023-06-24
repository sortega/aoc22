package aoc

object Day06:
  type Input = String

  def offsetAfterMarker(input: Input, size: Int): Int =
    input.sliding(size).indexWhere(allCharsDifferent) + size

  def allCharsDifferent(string: String): Boolean = string.toSet.size == string.length

  def part1(input: Input): Int = offsetAfterMarker(input, size = 4)

  def part2(input: Input): Int = offsetAfterMarker(input, size = 14)

  def main(args: Array[String]): Unit =
    val input = Inputs(day = 6).slurp
    println(part1(input))
    println(part2(input))
