package aoc

import scala.io.Source

object Inputs:
  def linesIterator(day: Int): Iterator[String] =
    Source.fromResource(f"day$day%02d.txt").getLines()

  def apply(day: Int): String = linesIterator(day).mkString("\n")
