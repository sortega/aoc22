package aoc

object Day01:
  type Input = List[Inventory]
  type Inventory = List[Calories]
  type Calories = Int

  object Inventory:
    def parseInventory(lines: List[String]): Inventory = lines.map(_.toInt)

  def part1(inventory: Input): Calories = inventory.map(_.sum).max

  def part2(inventory: Input): Calories = inventory.map(_.sum).sorted.takeRight(3).sum

  def main(args: Array[String]): Unit =
    val input = Inputs(day = 1).groupsOfLines.map(Inventory.parseInventory)
    println(part1(input))
    println(part2(input))
