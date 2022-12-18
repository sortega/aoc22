package aoc

import aoc.Collections.mapForKeys
import cats.implicits.*

import scala.annotation.{tailrec, targetName}

object Day18:
  type Input = Droplet
  type Droplet = Set[Point]

  case class Point(
      x: Int,
      y: Int,
      z: Int,
    ):
    def +(other: Point): Point = Point(x + other.x, y + other.y, z + other.z)
    def max(other: Point): Point = Point(x `max` other.x, y `max` other.y, z `max` other.z)
    def adjacent: Set[Point] = Point.Directions.map(_ + this)
    def toPos: Pos = Pos(row = -y, col = x)

  object Point:
    private val Directions = Set(
      Point(-1, 0, 0),
      Point(1, 0, 0),
      Point(0, -1, 0),
      Point(0, 1, 0),
      Point(0, 0, -1),
      Point(0, 0, 1),
    )

    def parse(s: String): Point =
      val Array(x, y, z) = s.split(",").nn.map(_.nn.toInt)
      Point(x, y, z)

  private def boundingBox(droplet: Droplet): Point = droplet.foldLeft(Point(0, 0, 0))(_ `max` _)

  def tomography(droplet: Droplet): String =
    val box = boundingBox(droplet)
    Matrix
      .fromMap(
        droplet
          .map(point => point.toPos - Pos(row = box.y * point.z, col = 0) -> '#')
          .toMap,
        '.',
      )
      .toString("")

  def fill(droplet: Droplet): Droplet =
    val box = boundingBox(droplet)
    val wholeSpace = (for {
      x <- 0 to (box.x + 1)
      y <- 0 to (box.y + 1)
      z <- 0 to (box.z + 1)
    } yield Point(x, y, z)).toSet

    @tailrec
    def growEnclosure(active: Droplet, notGaps: Droplet): Droplet =
      if active.isEmpty then notGaps
      else
        val nextNotGaps = notGaps `union` active
        val nextActive = active
          .flatMap(_.adjacent)
          .diff(nextNotGaps)
          .filter { point =>
            point.x > 0 && point.y > 0 && point.z > 0 &&
            point.x <= box.x && point.y <= box.y && point.z <= box.z
          }
        growEnclosure(nextActive, nextNotGaps)

    val active = wholeSpace.filter { p =>
      p.x == 0 || p.y == 0 || p.z == 0 || p.x == (box.x + 1) || p.y == (box.y + 1) || p.z == (box.z + 1)
    }
    val enclosure = growEnclosure(active, droplet)
    val gaps = wholeSpace `diff` enclosure
    droplet `union` gaps

  def part1(input: Input): Int = input.toList.foldMap { point =>
    point.adjacent.count(neighbor => !input.contains(neighbor))
  }

  def part2(input: Input): Int =
    println(s"DROPLET:\n${tomography(input)}")
    val filled = fill(input)
    println(s"FILLED:\n${tomography(filled)}")
    part1(filled)

  def main(args: Array[String]): Unit =
    val input = Inputs(day = 18).parseLines(Point.parse).toSet
    println(part1(input))
    println(part2(input))
