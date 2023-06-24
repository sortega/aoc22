package aoc

import aoc.Parsing.parseOrThrow
import atto.*
import atto.Atto.*
import scala.math.Ordering.Implicits.infixOrderingOps

object Day13:
  type Input = List[(Data, Data)]

  object Input:
    val parser: Parser[Input] =
      many((Data.parser <~ skipWhitespace) ~ (Data.parser <~ skipWhitespace))

  enum Data:
    case IntData(value: Int)
    case ListData(values: List[Data])

    override def toString: String = this match
      case IntData(value) => value.toString
      case ListData(values) => values.mkString("[", ",", "]")

  object Data:
    given Ordering[Data] with
      def compare(left: Data, right: Data) = (left, right) match
        case (IntData(l), IntData(r)) => l.compare(r)
        case (l: IntData, r: ListData) => compare(ListData(List(l)), r)
        case (l: ListData, r: IntData) => compare(l, ListData(List(r)))
        case (ListData(l), ListData(r)) => Ordering[List[Data]].compare(l, r)

    given lexicographicOrder[A](using order: Ordering[A]): Ordering[List[A]] with
      def compare(left: List[A], right: List[A]) = (left, right) match
        case (Nil, Nil) => 0
        case (Nil, _) => -1
        case (_, Nil) => 1
        case (l :: ls, r :: rs) =>
          order.compare(l, r) match
            case 0 => compare(ls, rs)
            case other => other

    private def intDataParser = int.map(IntData.apply)
    private def listDataParser =
      (char('[') ~> sepBy(parser, char(',')) <~ char(']')).map(ListData.apply)
    def parser: Parser[Data] = intDataParser | listDataParser

  import Data.*

  def part1(input: Input): Int =
    input
      .zipWithIndex
      .collect { case ((left, right), index) if left < right => index + 1 }
      .sum

  private def divider(int: Int): Data = ListData(List(ListData(List(IntData(int)))))

  def part2(input: Input): Int =
    val dividerPackets = List(divider(2), divider(6))
    val allPackets = (dividerPackets ++ input.flatMap(_.toList)).sorted
    allPackets
      .zipWithIndex
      .collect {
        case (data, index) if dividerPackets.contains(data) => index + 1
      }
      .product

  def main(args: Array[String]): Unit =
    val input = Inputs(day = 13).parse(Input.parser)
    println(part1(input))
    println(part2(input))
