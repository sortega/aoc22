package aoc

import cats.implicits.*

object Day25:
  object Snafu:
    private val powers = LazyList.iterate(1L)(_ * 5L)
    private val numerals = Map('=' -> -2L, '-' -> -1L, '0' -> 0L, '1' -> 1L, '2' -> 2L)

    def parse(string: String): Long = string.reverse.toList.zip(powers).foldMap {
      case (digit, power) => numerals(digit) * power
    }

    def format(number: Long): String =
      val digits = List.unfold(number) { remainder =>
        remainder % 5 match
          case 0 if remainder == 0 => None
          case digit @ (0 | 1 | 2) => Some(digit.toString.head -> (remainder / 5))
          case 3 => Some('=' -> (remainder / 5 + 1))
          case 4 => Some('-' -> (remainder / 5 + 1))
      }
      if digits.isEmpty then "0" else digits.reverse.mkString("")

  def part1(input: String): String =
    val numbers = input.linesIterator.map(Snafu.parse).toList
    Snafu.format(numbers.sum)

  def main(args: Array[String]): Unit =
    val input = Inputs(day = 25).slurp
    println(part1(input))
