package aoc

import atto.Parser
import atto.Atto.*
import cats.implicits.*
import scala.io.Source

object Inputs:
  def apply(day: Int, test: Boolean = false): InputSource =
    InputSource(Source.fromResource("day%02d%s.txt".format(day, if (test) "-test" else "")))
  def apply(inline: String): InputSource = InputSource(Source.fromString(inline))

  class InputSource(source: Source):
    def slurp: String = linesIterator.mkString("\n")

    def linesIterator: Iterator[String] = source.getLines()

    def groupsOfLines: List[List[String]] = Collections.splitList(linesIterator.toList)(_.isBlank)

    def parse[A](parser: Parser[A]): A = parseOrThrow(parser)(slurp)

    def parseLines[A](parser: Parser[A]): List[A] = parseLines(parseOrThrow(parser))

    def parseLines[A](parse: String => A): List[A] = linesIterator.map(parse).toList

    private def parseOrThrow[A](parser: Parser[A])(string: String): A =
      parser
        .parseOnly(string)
        .either
        .valueOr(error => throw new IllegalArgumentException(error))
