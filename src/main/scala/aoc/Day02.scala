package aoc

import aoc.Day02.Outcome.{ Lose, Draw, Win }
import aoc.Day02.{ Outcome, Strategy }
import aoc.Day02.Strategy.{ Rock, Paper, Scissors }

object Day02:
  type Guide = List[EncryptedRound]
  object Guide:
    def parse(lines: Iterator[String]): Guide = lines.map(EncryptedRound.parse).toList

  val StrategyCode: Map[Char, Strategy] = Map(
    'A' -> Rock,
    'B' -> Paper,
    'C' -> Scissors,
    'X' -> Rock,
    'Y' -> Paper,
    'Z' -> Scissors,
  )
  val OutcomeCode: Map[Char, Outcome] = Map(
    'X' -> Lose,
    'Y' -> Draw,
    'Z' -> Win,
  )

  case class EncryptedRound(opponent: Char, other: Char):
    def decrypt1: Round1 = Round1(StrategyCode(opponent), StrategyCode(other))
    def decrypt2: Round2 = Round2(StrategyCode(opponent), OutcomeCode(other))

  object EncryptedRound:
    def parse(s: String): EncryptedRound = s.split(" ") match
      case Array(opponent, yours) => EncryptedRound(opponent.nn.head, yours.nn.head)
      case _ => ???

  case class Round1(opponentStrategy: Strategy, myStrategy: Strategy):
    def score: Int =
      val outcome = myStrategy.playAgainst(opponentStrategy)
      outcome.points + myStrategy.points

  case class Round2(opponentStrategy: Strategy, outcome: Outcome):
    def score: Int =
      val myStrategy = opponentStrategy.strategyProducing(outcome)
      myStrategy.points + outcome.points

  enum Outcome:
    case Lose, Draw, Win
    def points: Int = ordinal * 3

  enum Strategy:
    case Rock, Paper, Scissors
    def points: Int = ordinal + 1

    def playAgainst(other: Strategy): Outcome = (this, other) match
      case (left, right) if left == right => Outcome.Draw
      case (Rock, Scissors) | (Paper, Rock) | (Scissors, Paper) => Outcome.Win
      case _ => Outcome.Lose

    def strategyProducing(outcome: Outcome): Strategy =
      Strategy.values.find(_.playAgainst(this) == outcome).get

  def part1(guide: Guide): Int = guide.map(_.decrypt1.score).sum

  def part2(guide: Guide): Int = guide.map(_.decrypt2.score).sum

  def main(args: Array[String]): Unit =
    val input = Guide.parse(Inputs.linesIterator(day = 2))
    println(part1(input))
    println(part2(input))
