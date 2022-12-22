package aoc

import aoc.CardinalPoint.*
import aoc.Day22.*

final class Day22Test extends UnitTest:
  val input = Input.parse(
    """        ...#
      |        .#..
      |        #...
      |        ....
      |...#.......#
      |........#...
      |..#....#....
      |..........#.
      |        ...#....
      |        .....#..
      |        .#......
      |        ......#.
      |
      |10R5L5R10L4R5L5
      |""".stripMargin
  )

  "A cube" `should` "know about connectivity" in {
    val cube = Cube(4, testFaces)
    // A to B
    cube.next(PointOfView(Pos(6, 12), East)) should ===(PointOfView(Pos(9, 15), South))
    // B to A
    cube.next(PointOfView(Pos(9, 15), North)) should ===(PointOfView(Pos(6, 12), West))
    // C to D
    cube.next(PointOfView(Pos(12, 11), South)) should ===(PointOfView(Pos(8, 2), North))
    // D to C
    cube.next(PointOfView(Pos(8, 2), South)) should ===(PointOfView(Pos(12, 11), North))
  }

  "A field" `should` "navigate the plane" in {
    val person = PointOfView(input.field.startingPos)
    input.field.navigatePlane(person, input.instructions) should ===(
      PointOfView(Pos(6, 8), East)
    )
  }

  it `should` "navigate the cube" in {
    val person = PointOfView(input.field.startingPos)
    input.field.navigateCube(person, input.instructions) should ===(
      PointOfView(Pos(5, 7), North)
    )
  }

  "Part 1" `should` "find the password" in {
    part1(input) should ===(6032)
  }

  "Part 2" `should` "find the password" in {
    part2(input) should ===(5031)
  }
