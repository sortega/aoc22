package aoc

import aoc.Functions.iterate
import scala.util.chaining.*
import cats.implicits.*

object Day20:
  type Input = List[Long]
  type File = Vector[Value]

  case class Value(offset: Int, value: Long):
    def *(factor: Long) = copy(value = value * factor)

  def toFile(input: Input): File = input
    .zipWithIndex
    .map {
      case (value, offset) => Value(offset, value)
    }
    .toVector

  def mix(file: File): File =
    val ordered = file.sortBy(_.offset)
    ordered.foldLeft(file) { (current, value) =>
      val currentIndex = current.indexOf(value)
      shift(current, currentIndex)
    }

  def shift(file: File, index: Int): File =
    val (left, valueAndRight) = file.splitAt(index)
    val value = valueAndRight.head
    val right = valueAndRight.tail
    val newOffset = mod(left.size + value.value, file.size - 1)
    if newOffset == 0 then (left ++ right) :+ value
    else
      val (before, after) = (left ++ right).splitAt(newOffset)
      before ++ Vector(value) ++ after

  def mod(a: Long, b: Int): Int =
    val r = (a % b).toInt
    if r < 0 then r + b else r

  def extractCoordinates(file: File): List[Long] =
    val base = file.indexWhere(_.value == 0)
    List(1000, 2000, 3000).map { offset =>
      file((base + offset) % file.size).value
    }

  def part1(file: File): Long = extractCoordinates(mix(file)).sum

  def part2(file: File): Long =
    iterate(file.map(_ * 811589153L), times = 10)(mix).pipe(extractCoordinates).sum

  def main(args: Array[String]): Unit =
    val input = Inputs(day = 20).parseLines(_.toLong)
    val file = toFile(input)
    println(part1(file))
    println(part2(file))
