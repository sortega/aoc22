package aoc

import aoc.Day16.*
import aoc.Functions.timed

final class Day16Test extends UnitTest:
  val input =
    Inputs(
      """Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
        |Valve BB has flow rate=13; tunnels lead to valves CC, AA
        |Valve CC has flow rate=2; tunnels lead to valves DD, BB
        |Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE
        |Valve EE has flow rate=3; tunnels lead to valves FF, DD
        |Valve FF has flow rate=0; tunnels lead to valves EE, GG
        |Valve GG has flow rate=0; tunnels lead to valves FF, HH
        |Valve HH has flow rate=22; tunnel leads to valve GG
        |Valve II has flow rate=0; tunnels lead to valves AA, JJ
        |Valve JJ has flow rate=21; tunnel leads to valve II
        |""".stripMargin
    ).parseLines(Valve.parse)

  "Part 1" `should` "find how much pressure to release in 30 minutes" in {
    part1(input) should ===(1651)
  }

  "Part 2" `should` "work with an elephant" in {
    timed(part2(input)) should ===(1707)
  }
