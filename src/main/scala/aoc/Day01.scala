package aoc

type Calories = Int
type Inventory = List[List[Calories]]

object Calories:
  def parseInventory(lines: Iterator[String]): Inventory =
    val parsedLines = lines.map(_.toIntOption).toList
    Collections.splitList(parsedLines)(_.isEmpty).map(_.flatten)

object Day01:
  def part1(inventory: Inventory): Calories = inventory.map(_.sum).max

  def part2(inventory: Inventory): Calories = inventory.map(_.sum).sorted.takeRight(3).sum

  def main(args: Array[String]): Unit =
    val input = Calories.parseInventory(Inputs.linesIterator(day = 1))
    println(part1(input))
    println(part2(input))
