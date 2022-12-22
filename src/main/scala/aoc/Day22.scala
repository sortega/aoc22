package aoc

import aoc.CardinalPoint.*
import aoc.Collections.mapForKeys
import cats.implicits.*
import scala.util.chaining.*
import scala.annotation.tailrec

object Day22:
  case class Input(field: Field, instructions: List[Instruction])
  object Input:
    def parse(input: String): Input =
      val lines = input.linesIterator.toList
      Input(Field.parse(lines.dropRight(2)), Instruction.parseAll(lines.last))

  case class Face(flatPos: Pos, adjacent: Map[CardinalPoint, Conn]):
    def topLeftOffset(side: Int): Pos = flatPos.map(v => (v - 1) * side)

  case class Conn(
      face: Int,
      facing: CardinalPoint,
      flip: Boolean = false,
    )

  case class Cube(side: Int, faces: Map[Int, Face]):
    def next(pov: PointOfView): PointOfView =
      val currentFace = faceContaining(pov.pos)
      val relPos = pov.pos - currentFace.topLeftOffset(side)
      val nextRelPos = relPos + pov.dir.pos
      if nextRelPos.row >= 1 && nextRelPos.col >= 1 &&
          nextRelPos.row <= side && nextRelPos.col <= side
      then
        // Stay on the same face
        pov.copy(pos = pov.pos + pov.dir.pos)
      else
        val conn = currentFace.adjacent(pov.dir)
        val nextFace = faces(conn.face)
        val keptDimension = (pov.dir match
          case North | South => relPos.col
          case East | West => relPos.row
        ).pipe(v => if conn.flip then side + 1 - v else v)
        val nextRelPos = conn.facing match
          case North => Pos(row = side, col = keptDimension)
          case West => Pos(row = keptDimension, col = side)
          case South => Pos(row = 1, col = keptDimension)
          case East => Pos(row = keptDimension, col = 1)
        PointOfView(nextRelPos + nextFace.topLeftOffset(side), conn.facing)

    private def faceContaining(pos: Pos): Face =
      val scaledDownPos = pos.map(v => (v - 1) / side + 1)
      faces
        .collectFirst {
          case (_, face) if face.flatPos == scaledDownPos => face
        }
        .getOrElse(throw new Exception(s"No face for $pos"))

  val prodFaces: Map[Int, Face] = Map(
    1 -> Face(
      flatPos = Pos(row = 1, col = 2),
      adjacent = Map(
        North -> Conn(6, facing = East),
        East -> Conn(2, facing = East),
        South -> Conn(3, facing = South),
        West -> Conn(4, facing = East, flip = true),
      ),
    ),
    2 -> Face(
      flatPos = Pos(row = 1, col = 3),
      adjacent = Map(
        North -> Conn(6, facing = North),
        East -> Conn(5, facing = West, flip = true),
        South -> Conn(3, facing = West),
        West -> Conn(1, facing = West),
      ),
    ),
    3 -> Face(
      flatPos = Pos(row = 2, col = 2),
      adjacent = Map(
        North -> Conn(1, facing = North),
        East -> Conn(2, facing = North),
        South -> Conn(5, facing = South),
        West -> Conn(4, facing = South),
      ),
    ),
    4 -> Face(
      flatPos = Pos(row = 3, col = 1),
      adjacent = Map(
        North -> Conn(3, facing = East),
        East -> Conn(5, facing = East),
        South -> Conn(6, facing = South),
        West -> Conn(1, facing = East, flip = true),
      ),
    ),
    5 -> Face(
      flatPos = Pos(row = 3, col = 2),
      adjacent = Map(
        North -> Conn(3, facing = North),
        East -> Conn(2, facing = West, flip = true),
        South -> Conn(6, facing = West),
        West -> Conn(4, facing = West),
      ),
    ),
    6 -> Face(
      flatPos = Pos(row = 4, col = 1),
      adjacent = Map(
        North -> Conn(4, facing = North),
        East -> Conn(5, facing = North),
        South -> Conn(2, facing = South),
        West -> Conn(1, facing = South),
      ),
    ),
  )

  val testFaces: Map[Int, Face] = Map(
    1 -> Face(
      flatPos = Pos(row = 1, col = 3),
      adjacent = Map(
        North -> Conn(2, facing = South, flip = true),
        East -> Conn(6, facing = West, flip = true),
        South -> Conn(4, facing = South),
        West -> Conn(3, facing = South, flip = true),
      ),
    ),
    2 -> Face(
      flatPos = Pos(row = 2, col = 1),
      adjacent = Map(
        North -> Conn(1, facing = South, flip = true),
        East -> Conn(3, facing = East),
        South -> Conn(5, facing = North, flip = true),
        West -> Conn(6, facing = North),
      ),
    ),
    3 -> Face(
      flatPos = Pos(row = 2, col = 2),
      adjacent = Map(
        North -> Conn(1, facing = East),
        East -> Conn(4, facing = East),
        South -> Conn(5, facing = East, flip = true),
        West -> Conn(2, facing = West),
      ),
    ),
    4 -> Face(
      flatPos = Pos(row = 2, col = 3),
      adjacent = Map(
        North -> Conn(1, facing = North),
        East -> Conn(6, facing = South, flip = true),
        South -> Conn(5, facing = South),
        West -> Conn(3, facing = West),
      ),
    ),
    5 -> Face(
      flatPos = Pos(row = 3, col = 3),
      adjacent = Map(
        North -> Conn(4, facing = North),
        East -> Conn(6, facing = East),
        South -> Conn(2, facing = North, flip = true),
        West -> Conn(3, facing = North),
      ),
    ),
    6 -> Face(
      flatPos = Pos(row = 3, col = 4),
      adjacent = Map(
        North -> Conn(4, facing = West, flip = true),
        East -> Conn(1, facing = West, flip = true),
        South -> Conn(2, facing = East, flip = true),
        West -> Conn(5, facing = West),
      ),
    ),
  )

  case class Field(cells: Map[Pos, Cell]):
    private val height = cells.keys.map(_.row).max + 1
    private val width = cells.keys.map(_.col).max + 1
    private val cube =
      val side = (width `max` height) / 4
      Cube(side, if side == 4 then testFaces else prodFaces)
    private val flatAdjacent: Map[PointOfView, PointOfView] = (for {
      pos <- cells.keys
      dir <- CardinalPoint.values
      pov = PointOfView(pos, dir)
    } yield
      val line = LazyList.iterate(pos) { pos =>
        (pos + dir.pos).transform(row => wrap(row, height), col => wrap(col, width))
      }
      val nextPos = line.tail.find(cells.contains).get
      pov -> PointOfView(nextPos, dir)
    ).toMap

    private def wrap(value: Int, modulo: Int): Int =
      val remainder = value % modulo
      if remainder < 0 then remainder + modulo
      else remainder

    def startingPos: Pos = cells
      .collect {
        case (pos, Cell.Empty) if pos.row == 1 => pos
      }
      .minBy(_.col)

    def navigatePlane(pov: PointOfView, instructions: List[Instruction]): PointOfView =
      navigate(pov, instructions, flatAdjacent)

    def navigateCube(pov: PointOfView, instructions: List[Instruction]): PointOfView =
      navigate(pov, instructions, cube.next)

    @tailrec
    private def navigate(
        pov: PointOfView,
        instructions: List[Instruction],
        step: PointOfView => PointOfView,
      ): PointOfView =
//      println(toString(pov))
//      println()
      instructions match
        case Nil => pov
        case Instruction.Advance(0) :: otherInstructions =>
          navigate(pov, otherInstructions, step)
        case Instruction.Advance(amount) :: otherInstructions =>
          val nextPov = step(pov)
          if cells(nextPov.pos) == Cell.Wall then navigate(pov, otherInstructions, step)
          else
            navigate(
              nextPov,
              Instruction.Advance(amount - 1) :: otherInstructions,
              step,
            )
        case Instruction.Turn(Dir.Left) :: otherInstructions =>
          navigate(pov.copy(dir = pov.dir.turnCounterClockwise), otherInstructions, step)
        case Instruction.Turn(Dir.Right) :: otherInstructions =>
          navigate(pov.copy(dir = pov.dir.turnClockwise), otherInstructions, step)

    def toString(pov: PointOfView): String =
      val textCells =
        cells.view.mapValues(_.toString).toMap.updated(pov.pos, pov.dir.char.toString)
      Matrix.fromMap(textCells, emptyValue = " ").toString(sep = "")

    override def toString: String =
      Matrix.fromMap(cells.view.mapValues(_.toString).toMap, emptyValue = " ").toString(sep = "")

  object Field:
    def parse(lines: List[String]): Field = Field(
      (for {
        (line, rowIndex) <- lines.zipWithIndex
        (cell, colIndex) <- line.zipWithIndex
        pos = Pos(rowIndex + 1, colIndex + 1)
        if cell != ' '
      } yield cell match
        case '#' => pos -> Cell.Wall
        case '.' => pos -> Cell.Empty
      ).toMap
    )

  enum Cell(char: Char):
    case Empty extends Cell('.')
    case Wall extends Cell('#')
    override def toString: String = char.toString

  enum Dir:
    case Right, Left

  object Dir:
    def parse(s: String): Dir = values.find(_.toString.startsWith(s)).get

  enum Instruction:
    case Advance(steps: Int)
    case Turn(dir: Dir)

  object Instruction:
    def parseAll(line: String): List[Instruction] =
      def loop(line: String, acc: List[Instruction]): List[Instruction] =
        if line.isEmpty then acc.reverse
        else if line.head.isLetter then
          val (dir, rest) = line.span(_.isLetter)
          loop(rest, Turn(Dir.parse(dir)) :: acc)
        else
          val (steps, rest) = line.span(_.isDigit)
          loop(rest, Advance(steps.toInt) :: acc)
      loop(line, Nil)

  case class PointOfView(pos: Pos, dir: CardinalPoint = East):
    private val dirToPassword = Map(
      East -> 0,
      South -> 1,
      West -> 2,
      North -> 3,
    )
    def toPassword: Int = pos.row * 1000 + pos.col * 4 + dirToPassword(dir)

  def part1(input: Input): Int =
    val pov = PointOfView(input.field.startingPos)
    input.field.navigatePlane(pov, input.instructions).toPassword

  def part2(input: Input): Int =
    val pov = PointOfView(input.field.startingPos)
    input.field.navigateCube(pov, input.instructions).toPassword

  def main(args: Array[String]): Unit =
    val input = Input.parse(Inputs(day = 22).slurp)
    println(part1(input))
    println(part2(input))
