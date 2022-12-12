package aoc

import cats.*
import cats.implicits.*

object Day09:
  type Input = List[Command]

  case class Command(dir: CardinalPoint, amount: Int)

  object Command:
    private val dirMapping = Map(
      "U" -> CardinalPoint.North,
      "D" -> CardinalPoint.South,
      "L" -> CardinalPoint.West,
      "R" -> CardinalPoint.East,
    )

    def parse(string: String): Command = string.split(" ") match
      case Array(dir, amount) => Command(dirMapping(dir.nn), amount.nn.toInt)
      case _ => ???

  case class State(head: Pos, tail: List[Pos]):
    def moveHead(dir: CardinalPoint): State =
      val nextHead = head + dir.pos
      val nextTail = tail
        .scanLeft(nextHead) { (target, pos) =>
          moveTowards(pos, target)
        }
        .tail
      State(nextHead, nextTail)

    private def moveTowards(pos: Pos, target: Pos): Pos =
      if (target.isAdjacent8(pos)) pos
      else
        val shouldMoveDiagonally = (target - pos).manhattanNorm > 1
        pos + (target - pos).map { delta =>
          if (shouldMoveDiagonally || delta.abs > 1) delta.sign else 0
        }

  object State:
    def initial(size: Int): State = State(head = Pos.Origin, tail = List.fill(size - 1)(Pos.Origin))

  def trailSize(input: Input, size: Int): Int =
    val directions = input.flatMap(command => List.fill(command.amount)(command.dir))
    val states = directions
      .scanLeft(State.initial(size)) { (state, dir) =>
        state.moveHead(dir)
      }
    states.map(_.tail.last).toSet.size

  def part1(input: Input): Int = trailSize(input, size = 2)

  def part2(input: Input): Int = trailSize(input, size = 10)

  def main(args: Array[String]): Unit =
    val input = Inputs(day = 9).parseLines(Command.parse)
    println(part1(input))
    println(part2(input))
