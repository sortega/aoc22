package aoc

import atto.*, Atto.*
import cats.implicits.*

object Day05:
  type Crate = Char
  type Stack = Int

  case class Stacks(values: Map[Stack, List[Crate]]):
    def moveMany(
        amount: Int,
        from: Stack,
        to: Stack,
      ): Stacks =
      if (amount <= 0) this
      else move(from, to).moveMany(amount - 1, from, to)

    def moveBatch(
        amount: Int,
        from: Stack,
        to: Stack,
      ): Stacks =
      val (head, tail) = values(from).splitAt(amount)
      Stacks(values ++ Map(from -> tail, to -> (head ::: values(to))))

    def move(from: Stack, to: Stack): Stacks =
      Stacks(values ++ Map(from -> values(from).tail, to -> (values(from).head :: values(to))))

    def topString: String = values.toList.sortBy(_._1).map(_._2.head).mkString

    override def toString: String =
      values
        .toList
        .sortBy(_._1)
        .map { case (stack, crates) => s"$stack: $crates" }
        .mkString("\n") + "\n"

  object Stacks:
    val stackElem: Parser[Option[Crate]] = orElse(
      (char('[') ~> letter <~ char(']')).map(Some.apply).named("crate"),
      string("   ").as(None).named("empty"),
    )
    val stackRowsParser: Parser[List[List[Option[Crate]]]] =
      many(sepBy(stackElem, char(' ')) <~ char('\n'))
    val labelsParser: Parser[List[Stack]] =
      sepBy(
        many(horizontalWhitespace) ~> int <~ many(horizontalWhitespace),
        many(horizontalWhitespace),
      ).named("labels") <~ char('\n')

    val parser: Parser[Stacks] = for {
      matrix <- stackRowsParser
      paddedMatrix = matrix.map(_.padTo(matrix.size, None))
      labels <- labelsParser
    } yield Stacks(labels.zip(paddedMatrix.transpose.map(_.flatten)).toMap)

  case class Instruction(
      amount: Int,
      from: Stack,
      to: Stack,
    )
  object Instruction:
    val parser: Parser[Instruction] =
      (string("move ") ~> int, string(" from ") ~> int, string(" to ") ~> int)
        .mapN(Instruction.apply)

  case class Input(stacks: Stacks, instructions: List[Instruction])

  object Input:
    val parser: Parser[Input] = (
      Stacks.parser <~ string("\n"),
      sepBy(Instruction.parser, char('\n')),
    ).mapN(Input.apply)

  def part1(input: Input): String =
    val finalStacks = input.instructions.foldLeft(input.stacks) { (stack, instruction) =>
      stack.moveMany(instruction.amount, instruction.from, instruction.to)
    }
    finalStacks.topString

  def part2(input: Input): String =
    val finalStacks = input.instructions.foldLeft(input.stacks) { (stack, instruction) =>
      stack.moveBatch(instruction.amount, instruction.from, instruction.to)
    }
    finalStacks.topString

  def main(args: Array[String]): Unit =
    val input = Inputs(day = 5).parse(Input.parser)
    println(part1(input))
    println(part2(input))
