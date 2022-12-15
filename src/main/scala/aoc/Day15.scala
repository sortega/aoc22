package aoc

import aoc.Day15.Input
import cats.implicits.*

import scala.util.chaining.*

object Day15:
  type Input = List[Sensor]

  case class Sensor(pos: Pos, closestBeacon: Pos):
    private val radius: Int = pos.manhattanDist(closestBeacon)

    def coverageAt(row: Int): Option[Range.Inclusive] =
      val width = radius - (pos.row - row).abs
      if width >= 0 then Some((pos.col - width) to (pos.col + width))
      else None

    def candidatePoints(searchRange: Range): Set[Pos] =
      val distance = radius + 1
      (for {
        deltaRow <- 0 to distance
        deltaCol = distance - deltaRow
        rowSign <- Set(-1, 1)
        colSign <- Set(-1, 1)
        row = pos.row + rowSign * deltaRow
        if searchRange.contains(row)
        col = pos.col + colSign * deltaCol
        if searchRange.contains(col)
      } yield Pos(row, col)).toSet

    def covers(target: Pos): Boolean = pos.manhattanDist(target) <= radius

  object Sensor:
    def parse(line: String): Sensor = line match
      case s"Sensor at x=$posCol, y=$posRow: closest beacon is at x=$beaconCol, y=$beaconRow" =>
        Sensor(Pos(posRow.toInt, posCol.toInt), Pos(beaconRow.toInt, beaconCol.toInt))

  def part1(input: Input, row: Int): Int =
    val nonEmptyPlaces = input.flatMap { sensor =>
      List(sensor.pos, sensor.closestBeacon).collect {
        case Pos(`row`, col) => col
      }
    }.toSet
    input
      .foldMap { sensor =>
        sensor
          .coverageAt(row)
          .map(_.toSet)
          .getOrElse(Set.empty)
      }
      .diff(nonEmptyPlaces)
      .size

  def part2(input: Input, size: Int): Long =
    val searchRange = 0 until size
    val candidates = input.foldMap(_.tap(println).candidatePoints(searchRange))
    encode(candidates.find { candidate =>
      input.forall(sensor => !sensor.covers(candidate))
    }.get)

  private def encode(pos: Pos): Long = pos.col * 4000000L + pos.row

  def main(args: Array[String]): Unit =
    val input = Inputs(day = 15).parseLines(Sensor.parse)
    println(part1(input, row = 2000000))
    println(part2(input, size = 4000000))
