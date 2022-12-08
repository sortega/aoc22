package aoc

import scala.annotation.targetName

case class Pos(row: Int, col: Int):
  @targetName("plus")
  def +(other: Pos): Pos = Pos(row + other.row, col + other.col)
  @targetName("minus")
  def -(other: Pos): Pos = Pos(row - other.row, col - other.col)
