package aoc

import atto.*
import atto.Atto.*
import cats.*
import cats.data.*
import cats.implicits.*

object Day10:
  type Input = List[Command]
  type Signal = Vector[Int]
  type LoggingSignal[A] = Writer[Signal, A]

  enum Command:
    case Noop
    case AddX(value: Int)

  object Command:
    def parser: Parser[Command] =
      string("noop").as(Noop) | (string("addx ") ~> int).map(AddX.apply)

  case class Machine(x: Int):
    def execute(command: Command): LoggingSignal[Machine] =
      command match
        case Command.Noop => this.writer(Vector(x))
        case Command.AddX(value) =>
          val next = copy(x + value)
          next.writer(Vector(x, x))

    def execute(commands: List[Command]): LoggingSignal[Machine] =
      commands.foldLeftM(this)(_.execute(_))

  val Lit = '#'
  val Dark = '.'
  val ScreenWidth = 40

  def display(signal: Signal): String =
    signal.grouped(ScreenWidth).map(displayLine).mkString("\n")

  def displayLine(line: Signal): String =
    line
      .zipWithIndex
      .map { (spritePos, index) =>
        if ((index - spritePos).abs < 2) Lit else Dark
      }
      .mkString

  object Machine:
    val Initial = Machine(x = 1)

  def part1(input: Input): Int =
    val signal = Machine.Initial.execute(input).written
    val checkpoints = List(20, 60, 100, 140, 180, 220)
    checkpoints.foldMap(checkpoint => checkpoint * signal(checkpoint - 1))

  def part2(input: Input): String =
    val signal = Machine.Initial.execute(input).written
    display(signal)

  def main(args: Array[String]): Unit =
    val input = Inputs(day = 10).parseLines(Command.parser)
    println(part1(input))
    println(part2(input))
