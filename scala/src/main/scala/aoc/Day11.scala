package aoc

import atto.*
import atto.Atto.*
import cats.*
import cats.data.*
import cats.implicits.*
import scala.collection.immutable.Queue
import scala.util.chaining.*

object Day11:
  type Input = MonkeyTroop
  type Item = Long
  type Activity = Map[Int, Int]
  type WithActivity[A] = Writer[Activity, A]

  def debug(message: => Any): Unit = {}

  case class MonkeyTroop(monkeys: Map[Int, Monkey]):
    val indices = monkeys.keys.toList.sorted
    val modulo = monkeys.values.map(_.test.divisor).product

    def nextRound(cooldown: Boolean): WithActivity[MonkeyTroop] =
      indices.foldLeftM(this) { (troop, index) =>
        debug(s"Monkey $index:")
        troop.runTurn(index, cooldown)
      }

    def runTurn(index: Int, cooldown: Boolean): WithActivity[MonkeyTroop] =
      monkeys(index).run(cooldown) match
        case None => this.pure
        case Some((updatedMonkey, send)) =>
          val normalizedItem = if (cooldown) send.item else send.item % modulo
          update(index, _ => updatedMonkey)
            .update(send.toMonkey, _.receive(normalizedItem))
            .writer(Map(index -> 1))
            .flatMap(_.runTurn(index, cooldown))

    def update(index: Int, f: Monkey => Monkey): MonkeyTroop = copy(
      monkeys.updatedWith(index)(maybeMonkey => Some(f(maybeMonkey.get)))
    )

    override def toString: String = indices
      .map { index =>
        s"Monkey $index: ${monkeys(index).items.mkString(", ")}"
      }
      .mkString("\n")

  object MonkeyTroop:
    val parser: Parser[MonkeyTroop] = sepBy(
      (
        string("Monkey ") ~> int <~ string(":\n"),
        Monkey.parser,
      ).tupled,
      whitespace,
    ).map(entries => MonkeyTroop(entries.toMap))

  case class Monkey(
      items: Queue[Item],
      operation: Operation,
      test: Test,
    ):
    def receive(item: Item): Monkey = copy(items = items.enqueue(item))

    def run(cooldown: Boolean): Option[(Monkey, Send)] =
      items.dequeueOption.map { (item, rest) =>
        debug(s"  Monkey inspects an item with a worry level of $item.")
        val nextItem = operation(item).pipe(if (cooldown) getBored else identity)
        val nextMonkey = test(nextItem)
        debug(s"    Item with worry level $nextItem is thrown to monkey $nextMonkey.")
        copy(items = rest) -> Send(nextItem, nextMonkey)
      }

    private def getBored(item: Item): Item =
      debug(s"    Monkey gets bored with item. Worry level is divided by 3 to ${item / 3}.")
      item / 3

  object Monkey:
    private val startingItemsParser =
      string("  Starting items: ") ~> sepBy(long, string(", ")).map(_.to(Queue)) <~ char('\n')

    val parser: Parser[Monkey] = (
      startingItemsParser,
      Operation.parser,
      Test.parser,
    ).mapN(Monkey.apply)

  case class Test(
      divisor: Int,
      whenTrue: Int,
      whenFalse: Int,
    ):
    def apply(item: Item): Int =
      val divisible = item % divisor == 0
      debug(
        s"    Current worry level is${if (divisible) "" else " not"} divisible by ${divisor}."
      )
      if (divisible) whenTrue else whenFalse

  object Test:
    val parser: Parser[Test] = (
      string("  Test: divisible by ") ~> int <~ char('\n'),
      string("    If true: throw to monkey ") ~> int <~ char('\n'),
      string("    If false: throw to monkey ") ~> int <~ char('\n'),
    ).mapN(Test.apply)

  enum Operation:
    case Multiply(factor: Int)
    case Add(addend: Int)
    case Square

    def apply(old: Item): Item = this match
      case Multiply(factor) =>
        debug(s"    Worry level is multiplied by $factor to ${old * factor}.")
        old * factor
      case Add(addend) =>
        debug(s"    Worry level is increased by $addend to ${old + addend}.")
        old + addend
      case Square =>
        debug(s"    Worry level is multiplied by itself to ${old * old}.")
        old * old

  object Operation:
    val parser: Parser[Operation] =
      string("  Operation: new = old ") ~> (
        string("* old").as(Square) |
          (string("* ") ~> int).map(Multiply.apply) |
          (string("+ ") ~> int).map(Add.apply)
      ) <~ char('\n')

  case class Send(item: Item, toMonkey: Int)

  def monkeyBusinessLevel(
      troop: MonkeyTroop,
      rounds: Int,
      cooldown: Boolean,
    ): Long =
    val activity = Functions.iterateM(troop, rounds)(_.nextRound(cooldown)).written
    val mostActive = activity.values.toList.sorted.takeRight(2)
    mostActive.map(_.toLong).product

  def part1(input: Input): Long = monkeyBusinessLevel(input, rounds = 20, cooldown = true)

  def part2(input: Input): Long = monkeyBusinessLevel(input, rounds = 10000, cooldown = false)

  def main(args: Array[String]): Unit =
    val input = Inputs(day = 11).parse(MonkeyTroop.parser)
    println(part1(input))
    println(part2(input))
