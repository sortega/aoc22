package aoc

import scala.io.Source

object Input:
  def apply(day: Int): InputSource = InputSource(Source.fromResource(f"day$day%02d.txt"))
  def apply(inline: String): InputSource = InputSource(Source.fromString(inline))

  class InputSource(source: Source):
    def slurp: String = linesIterator.mkString("\n")

    def linesIterator: Iterator[String] = source.getLines()
    
    def groupsOfLines: List[List[String]] = Collections.splitList(linesIterator.toList)(_.isBlank)

    def parseLines[A](parse: String => A): List[A] = linesIterator.map(parse).toList
