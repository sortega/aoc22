package aoc

import atto.*
import atto.Atto.*
import cats.*
import cats.implicits.*
import aoc.CardinalPoint.*

import scala.annotation.tailrec

object Day08:
  type Forest = Matrix[Tree]
  object Forest:
    def parse(source: Inputs.InputSource): Forest = Matrix(
      source.parseLines(_.map(_.toString.toInt).toVector).toVector
    )

  type Tree = Int
  type Row = Vector[Tree]
  type Visibility = Matrix[Boolean]

  def castShadowsFromWest(row: Row): Row =
    row.scanLeft(Int.MinValue)(_ `max` _).init

  def computeVisibility(forest: Forest): Visibility =
    val directionalShadows = inAllDirections(forest)(castShadowsFromWest)
    val shadow = directionalShadows.values.reduce((l, r) => l.zipWith(r)(_ `min` _))
    forest.zipWith(shadow)((height, shadow) => height > shadow)

  def inAllDirections(forest: Forest)(fromWest: Row => Row): Map[CardinalPoint, Forest] =
    Collections.mapForKeys(CardinalPoint.values) {
      case West => forest.mapRows(fromWest)
      case East => forest.reverseRows.mapRows(fromWest).reverseRows
      case South => forest.mapColumns(fromWest)
      case North => forest.transpose.reverseRows.mapRows(fromWest).reverseRows.transpose
    }

  def computeScenicScores(forest: Forest): Matrix[Int] =
    val directionalViews = inAllDirections(forest) { row =>
      for {
        (height, index) <- row.zipWithIndex
        lowerTrees = row.segmentLength(_ < height, index + 1)
        sameHeightTree = index + 1 + lowerTrees < row.size
      } yield lowerTrees + (if (sameHeightTree) 1 else 0)
    }
    directionalViews.values.reduce(_ `dotProduct` _)

  def part1(input: Forest): Int = computeVisibility(input).values.count(identity)

  def part2(input: Forest): Int = computeScenicScores(input).values.max

  def main(args: Array[String]): Unit =
    val input = Forest.parse(Inputs(day = 8))
    println(part1(input))
    println(part2(input))
