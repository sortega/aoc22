package aoc

import aoc.Day21.*

final class Day21Test extends UnitTest:
  val input = Purpose.parse("""root: pppw + sjmn
                              |dbpl: 5
                              |cczh: sllz + lgvd
                              |zczc: 2
                              |ptdq: humn - dvpt
                              |dvpt: 3
                              |lfqf: 4
                              |humn: 5
                              |ljgn: 2
                              |sjmn: drzm * dbpl
                              |sllz: 4
                              |pppw: cczh / lfqf
                              |lgvd: ljgn * ptdq
                              |drzm: hmdt - zczc
                              |hmdt: 32
                              |""".stripMargin)

  "Part 1" `should` "find out what the root monkey will yell" in {
    part1(input) should ===(152L)
  }

  "Part 2" `should` "work" in {
    part2(input) should ===(301L)
//    part2(actualInput) should ===(0)
  }
