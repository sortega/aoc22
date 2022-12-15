package aoc

import aoc.Day15.*

final class Day15Test extends UnitTest:
  val input =
    Inputs(
      """Sensor at x=2, y=18: closest beacon is at x=-2, y=15
        |Sensor at x=9, y=16: closest beacon is at x=10, y=16
        |Sensor at x=13, y=2: closest beacon is at x=15, y=3
        |Sensor at x=12, y=14: closest beacon is at x=10, y=16
        |Sensor at x=10, y=20: closest beacon is at x=10, y=16
        |Sensor at x=14, y=17: closest beacon is at x=10, y=16
        |Sensor at x=8, y=7: closest beacon is at x=2, y=10
        |Sensor at x=2, y=0: closest beacon is at x=2, y=10
        |Sensor at x=0, y=11: closest beacon is at x=2, y=10
        |Sensor at x=20, y=14: closest beacon is at x=25, y=17
        |Sensor at x=17, y=20: closest beacon is at x=21, y=22
        |Sensor at x=16, y=7: closest beacon is at x=15, y=3
        |Sensor at x=14, y=3: closest beacon is at x=15, y=3
        |Sensor at x=20, y=1: closest beacon is at x=15, y=3
        |""".stripMargin
    ).parseLines(Sensor.parse)

  "A sensor" `should` "compute its coverage for a row" in {
    val sensor = Sensor(Pos.Origin, closestBeacon = Pos(2, 3))
    sensor.coverageAt(0) should ===(Some(-5 to 5))
    sensor.coverageAt(-1) should ===(Some(-4 to 4))
    sensor.coverageAt(5) should ===(Some(0 to 0))
    sensor.coverageAt(-6) should ===(None)
    sensor.coverageAt(6) should ===(None)
  }

  it `should` "have candidates just outside its coverage" in {
    Sensor(Pos.Origin, closestBeacon = Pos(0, 0)).candidatePoints(0 until 20) should ===(
      Set(Pos(0, 1), Pos(1, 0))
    )
  }

  "Part 1" `should` "count cells" in {
    part1(input, row = 10) should ===(26)
  }

  "Part 2" `should` "find the beacon" in {
    part2(input, size = 20) should ===(56000011L)
  }
