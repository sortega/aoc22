package aoc

import cats.implicits.*
import scala.util.chaining.*

object Day03:
  type Input = List[Rucksack]

  type Item = Char
  type Compartment = String

  val priorities: String = (('a' to 'z') ++ ('A' to 'Z')).mkString

  def priorityOf(item: Item): Int = priorities.indexOf(item) + 1

  case class Rucksack(first: Compartment, second: Compartment):
    def itemSet: Set[Item] = (first + second).toSet
    def itemsInBothCompartments: Set[Item] =
      first.toSet `intersect` second.toSet

  object Rucksack:
    def parse(line: String): Rucksack =
      val (first, second) = line.splitAt(line.length / 2)
      Rucksack(first, second)

  def part1(input: Input): Int = input.foldMap { rucksack =>
    rucksack.itemsInBothCompartments.head.pipe(priorityOf)
  }

  def part2(input: Input): Int = input.grouped(3).toList.foldMap { group =>
    val badge = group.map(_.itemSet).reduce(_ `intersect` _).head
    priorityOf(badge)
  }

  def main(args: Array[String]): Unit =
    val input = Input(day = 3).parseLines(Rucksack.parse)
    println(part1(input))
    println(part2(input))
