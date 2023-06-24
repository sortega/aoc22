package aoc

import aoc.Collections.mapForKeys
import aoc.Day17.Cell.*
import aoc.Functions.{ iterate, timed }
import cats.implicits.*

import scala.annotation.tailrec
import scala.util.chaining.*

// This day, Pos.col grows to to top rather than to the bottom
object Day17:
  type Input = JestPattern
  case class JestPattern(values: List[Dir])
  object JestPattern:
    def parse(input: String): JestPattern = JestPattern(input.trim.nn.toList.map {
      case '<' => Dir.Left
      case '>' => Dir.Right
    })

  enum Dir(val vector: Pos):
    case Left extends Dir(Pos(0, -1))
    case Right extends Dir(Pos(0, 1))

  case class Rock(cells: Set[Pos]):
    val width: Int = cells.map(_.col).max + 1
    val height: Int = cells.map(_.row).max + 1
    def moveTo(pos: Pos): Rock = Rock(cells.map(_ + pos))

  val Rocks: Vector[Rock] = Vector(
    // format: off
    // -
    Rock(Set(Pos(0,0), Pos(0,1), Pos(0,2), Pos(0,3))),
    // +
    Rock(Set(
                 Pos(2,1),
      Pos(1, 0), Pos(1,1), Pos(1, 2),
                 Pos(0,1),
    )),
    // _|
    Rock(Set(
                           Pos(2, 2),
                           Pos(1, 2),
      Pos(0, 0), Pos(0,1), Pos(0, 2),
    )),
    // |
    Rock(Set(
      Pos(3, 0),
      Pos(2, 0),
      Pos(1, 0),
      Pos(0, 0),
    )),
    // o
    Rock(Set(
      Pos(1, 0), Pos(1, 1),
      Pos(0, 0), Pos(0, 1),
    ))
    // format: on
  )

  private val CaveWidth = 7

  case class TopState(
      consolidated: Set[Pos] = Set.empty,
      jestIndex: Int,
      rockIndex: Int,
    )

  case class Cave(
      consolidated: Set[Pos] = Set.empty,
      time: Int = 0,
      rocks: Int = 0,
      jestPattern: JestPattern,
      offset: Int = 0,
    ):
    def height: Int = offset + consolidatedHeight

    private def consolidatedHeight: Int = consolidated.map(_.row).maxOption.getOrElse(0)

    private def jestIndex: Int = time % jestPattern.values.size

    private def normalized: Cave =
      val removedHeight = (1 to height)
        .findLast { row =>
          (0 until CaveWidth).forall(col => consolidated.contains(Pos(row, col)))
        }
        .getOrElse(0)
      copy(
        consolidated = consolidated.collect {
          case pos if pos.row > removedHeight => pos - Pos(removedHeight, 0)
        },
        offset = offset + removedHeight,
      )

    private def collides(cells: Set[Pos]): Boolean =
      cells.exists { pos =>
        pos.col < 0 || pos.col >= CaveWidth || pos.row < 1 || consolidated.contains(pos)
      }

    def dropRock: Cave =
      val rock = Rocks(rocks % Rocks.size)
      val initialPos = Pos(row = consolidatedHeight + 4, col = 2)

      @tailrec
      def step(pos: Pos, time: Int): Cave =
        val dir = jestPattern.values(time % jestPattern.values.size)
        val jettedPos = (pos + dir.vector).pipe { jettedPos =>
          if !collides(rock.moveTo(jettedPos).cells) then jettedPos
          else pos
        }

        // Fall or stop
        val fallenPos = jettedPos + Pos(-1, 0)
        if collides(rock.moveTo(fallenPos).cells) then
          copy(
            consolidated = consolidated.union(rock.moveTo(jettedPos).cells),
            time = time + 1,
            rocks = rocks + 1,
          )
        else step(fallenPos, time = time + 1)

      step(initialPos, time).normalized

    def topState: TopState = TopState(consolidated, jestIndex, rocks % Rocks.size)

    override def toString: String =
      val floor = Set.tabulate(CaveWidth)(col => Pos(0, col))
      s"Time=$time Height=$height Jest=$jestIndex Rock=$rocks\n" + Matrix
        .fromSet(consolidated.union(floor).map(pos => pos.invertRow), filled = Rocky, empty = Empty)
        .toString("")

  enum Cell(char: Char):
    case Empty extends Cell('.')
    case Rocky extends Cell('#')
    override def toString: String = char.toString

  def part1(input: Input): Long =
    iterate(Cave(jestPattern = input), 2022)(_.dropRock).height

  case class Cycle(
      seed: Cave,
      start: Cave,
      end: Cave,
    ):
    private val period: Int = end.rocks - start.rocks
    private val heightPerCycle: Int = end.height - start.height

    def heightAt(index: Long): Long =
      if index < start.rocks
      then iterate(seed, index.toInt)(_.dropRock).height
      else
        val smallerIndex = (index - start.rocks) % period
        val nonPeriodicHeight = iterate(start, smallerIndex.toInt)(_.dropRock).height
        val periodicHeight = (index - start.rocks) / period * heightPerCycle
        nonPeriodicHeight + periodicHeight

  object Cycle:
    def detect(cave: Cave): Cycle = LazyList
      .iterate(CycleDetectionState.from(cave))(_.step)
      .flatMap(_.toCycle)
      .head

  case class CycleDetectionState(
      seed: Cave,
      cave: Cave,
      seen: Map[TopState, Cave],
    ):
    def step: CycleDetectionState = copy(
      cave = cave.dropRock,
      seen = seen.updated(cave.topState, cave),
    )

    def toCycle: Option[Cycle] = seen.get(cave.topState).map(start => Cycle(seed, start, cave))

  object CycleDetectionState:
    def from(seed: Cave): CycleDetectionState =
      CycleDetectionState(seed, cave = seed, seen = Map.empty)

  private def heightAfterIterations(input: Input, iterations: Long): Long =
    Cycle.detect(Cave(jestPattern = input)).heightAt(iterations)

  def part2(input: Input): Long = heightAfterIterations(input, 1000000000000L)

  def main(args: Array[String]): Unit =
    val input = JestPattern.parse(Inputs(day = 17).slurp)
    println(timed(part1(input)))
    println(timed(part2(input)))
