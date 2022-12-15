package aoc

import scala.annotation.targetName

case class Pos(row: Int, col: Int):
  @targetName("plus")
  def +(other: Pos): Pos = Pos(row + other.row, col + other.col)
  @targetName("minus")
  def -(other: Pos): Pos = Pos(row - other.row, col - other.col)

  def map(f: Int => Int): Pos = Pos(f(row), f(col))

  def adjacent4: Set[Pos] = CardinalPoint.values.map(_.pos + this).toSet

  def isAdjacent8(other: Pos): Boolean =
    val diff = other - this
    diff.row.abs <= 1 && diff.col.abs <= 1

  def manhattanNorm: Int = row.abs + col.abs
  def manhattanDist(other: Pos): Int = (other - this).manhattanNorm

object Pos:
  val Origin = Pos(0, 0)
