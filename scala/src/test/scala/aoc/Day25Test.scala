package aoc

import aoc.Day25.*
import org.scalatest.prop.TableDrivenPropertyChecks

final class Day25Test extends UnitTest with TableDrivenPropertyChecks:
  val input =
    """1=-0-2
      |12111
      |2=0=
      |21
      |2=01
      |111
      |20012
      |112
      |1=-1=
      |1-12
      |12
      |1=
      |122
      |""".stripMargin
  val actualInput = Inputs(day = 25).slurp

  "SNAFU numerals" `should` "be parsed and formatted" in {
    val pairs = Table[String, Long](
      "snafu" -> "decimal",
      "1=-0-2" -> 1747,
      "12111" -> 906,
      "2=0=" -> 198,
      "21" -> 11,
      "2=01" -> 201,
      "111" -> 31,
      "20012" -> 1257,
      "112" -> 32,
      "1=-1=" -> 353,
      "1-12" -> 107,
      "12" -> 7,
      "1=" -> 3,
      "122" -> 37,
      "2=-1=0" -> 4890,
    )
    forAll(pairs) { (snafu, decimal) =>
      Snafu.parse(snafu) should ===(decimal)
      Snafu.format(decimal) should ===(snafu)
    }
  }

  "Part 1" `should` "add up snafu numbers" in {
    part1(input) should ===("2=-1=0")
    part1(actualInput) should ===("2-=0-=-2=111=220=100")
  }
