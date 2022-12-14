package aoc

import aoc.Collections.mapForKeys
import aoc.Inputs.InputSource
import cats.data.NonEmptyList
import cats.implicits.*

import scala.annotation.tailrec

object Day14:
  private val EntryPoint = Pos(row = 0, col = 500)
  private val SandMoves = List(Pos(1, 0), Pos(1, -1), Pos(1, 1))

  type Input = Cave

  case class Cave(contents: Map[Pos, Cell], floorRow: Option[Int] = None):
    val maxRow = contents.keys.map(_.row).max

    def countSand: Int = contents.values.count(_ == Cell.Sand)

    def withFloor: Cave = copy(floorRow = Some(maxRow + 2))

    def blockedEntry: Boolean = contents.get(EntryPoint).nonEmpty

    def produceAllSandPossible: Cave =
      @tailrec def loop(cave: Cave): Cave =
        if cave.blockedEntry then cave
        else
          cave.produceSand match
            case None => cave
            case Some(nextCave) => loop(nextCave)

      loop(this)

    def produceSand: Option[Cave] =
      @tailrec
      def restingPos(pos: Pos): Option[Pos] =
        if floorRow.isEmpty && pos.row > maxRow then None
        else
          val candidates = SandMoves.map(_ + pos)
          val maybePos = candidates.find { candidatePos =>
            contents.get(candidatePos).isEmpty && floorRow.forall(_ != candidatePos.row)
          }
          maybePos match
            case None => Some(pos)
            case Some(nextPos) => restingPos(nextPos)

      restingPos(EntryPoint).map { pos =>
        copy(contents = contents.updated(pos, Cell.Sand))
      }

    override def toString: String =
      val min = Pos(
        row = contents.keys.map(_.row).min,
        col = contents.keys.map(_.col).min,
      )
      val normalizedContents = contents.map {
        case (pos, cell) => (pos - min, cell)
      }
      Matrix.fromMap(normalizedContents, Cell.Air).toString(sep = "")

  object Cave:
    def parse(source: InputSource): Cave =
      val rockLines = source.parseLines { line =>
        raw"(\d+),(\d+)"
          .r
          .findAllMatchIn(line)
          .map(m => Pos(m.group(2).toInt, m.group(1).toInt))
          .toList
      }
      val rocks =
        rockLines.foldMap { rockLine =>
          rockLine
            .sliding(2)
            .flatMap {
              case List(from, to) => line(from, to)
              case _ => ???
            }
            .toSet
        }
      Cave(mapForKeys(rocks)(_ => Cell.Rock))

    def line(start: Pos, end: Pos): Set[Pos] =
      val dir = (end - start).map(_.sign)
      LazyList.iterate(start)(_ + dir).takeWhile(_ != end).toSet + end

  enum Cell(char: Char):
    case Air extends Cell('.')
    case Sand extends Cell('o')
    case Rock extends Cell('#')

    override def toString = char.toString

  def part1(input: Input): Int = input.produceAllSandPossible.countSand

  def part2(input: Input): Int = input.withFloor.produceAllSandPossible.countSand

  def main(args: Array[String]): Unit =
    val input = Cave.parse(Inputs(day = 14))
    println(part1(input))
    println(part2(input))
