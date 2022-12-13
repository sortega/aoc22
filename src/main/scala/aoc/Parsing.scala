package aoc

import atto.Parser
import atto.Atto.*
import cats.implicits.*

object Parsing:
  def parseOrThrow[A](parser: Parser[A])(string: String): A =
    parser
      .parseOnly(string)
      .either
      .valueOr(error => throw new IllegalArgumentException(error))
