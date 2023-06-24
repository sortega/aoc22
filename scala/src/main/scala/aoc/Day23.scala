package aoc

import cats.implicits.*
import aoc.CardinalPoint.*
import aoc.CompassDir.*
import aoc.Functions.iterate

object Day23:
  type Input = GameOfElfs

  case class Rule(move: CardinalPoint, freeSpots: Set[CompassDir])
  object Rule:
    val all = List(
      Rule(move = North, freeSpots = Set(N, NE, NW)),
      Rule(move = South, freeSpots = Set(S, SE, SW)),
      Rule(move = West, freeSpots = Set(W, NW, SW)),
      Rule(move = East, freeSpots = Set(E, NE, SE)),
    )

    def rulesAt(time: Int): List[Rule] = Collections.rotate(all, time)

  case class GameOfElfs(elves: Set[Pos], time: Int = 0):
    def step(times: Int): GameOfElfs = iterate(this, times)(_.step)

    def step: GameOfElfs =
      val rules = Rule.rulesAt(time)

      val proposedMoves: Map[Pos, Pos] = (for {
        elfPos <- elves
        if elfPos.adjacent8.exists(elves.contains)
        applicableRule <- rules.find(rule =>
          rule.freeSpots.forall(dir => !elves.contains(elfPos + dir.pos))
        )
        proposedPos = elfPos + applicableRule.move.pos
      } yield elfPos -> proposedPos).toMap

      val proposalsPerPos: Map[Pos, Int] = proposedMoves.groupMapReduce(_._2)(_ => 1)(_ + _)

      val nextElfPositions: Set[Pos] = elves.map { elfPos =>
        (for {
          proposedPos <- proposedMoves.get(elfPos)
          if proposalsPerPos(proposedPos) == 1
        } yield proposedPos).getOrElse(elfPos) // Keep in place
      }

      copy(elves = nextElfPositions, time = time + 1)

    def emptyCellsInBoundingBox: Int = toMatrix.values.count(_ == '.')

    def toMatrix: Matrix[Char] = Matrix.fromSet(elves, filled = '#', empty = '.')

    override def toString: String =
      s"Time: $time\n${toMatrix.toString(sep = "")}"

  object GameOfElfs:
    def parse(input: String): GameOfElfs =
      GameOfElfs(Matrix.fromString(input).toMap.collect { case (pos, '#') => pos }.toSet)

  def part1(input: Input): Int = input.step(times = 10).emptyCellsInBoundingBox

  def part2(input: Input): Int = LazyList
    .iterate(input)(_.step)
    .sliding(2)
    .collectFirst {
      case LazyList(prev, next) if prev.elves == next.elves => next.time
    }
    .get

  def main(args: Array[String]): Unit =
    val input = GameOfElfs.parse(Inputs(day = 23).slurp)
    println(part1(input))
    println(part2(input))
