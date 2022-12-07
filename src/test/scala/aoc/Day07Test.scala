package aoc

import aoc.Day07.*
import aoc.Day07.Command.*
import aoc.Day07.ListEntry.*

final class Day07Test extends UnitTest:
  val input = Inputs(
    """$ cd /
      |$ ls
      |dir a
      |14848514 b.txt
      |8504156 c.dat
      |dir d
      |$ cd a
      |$ ls
      |dir e
      |29116 f
      |2557 g
      |62596 h.lst
      |$ cd e
      |$ ls
      |584 i
      |$ cd ..
      |$ cd ..
      |$ cd d
      |$ ls
      |4060174 j
      |8033020 d.log
      |5626152 d.ext
      |7214296 k
      |""".stripMargin
  ).parse(Listing.parser)

  "The parser" `should` "work for the sample input" in {
    input should ===(
      List(
        ChangeDir(Target.Root),
        ListDir(
          List(
            DirEntry("a"),
            FileEntry(14848514, "b.txt"),
            FileEntry(8504156, "c.dat"),
            DirEntry("d"),
          )
        ),
        ChangeDir(Target.Down("a")),
        ListDir(
          List(
            DirEntry("e"),
            FileEntry(29116, "f"),
            FileEntry(2557, "g"),
            FileEntry(62596, "h.lst"),
          )
        ),
        ChangeDir(Target.Down("e")),
        ListDir(List(FileEntry(584, "i"))),
        ChangeDir(Target.Up),
        ChangeDir(Target.Up),
        ChangeDir(Target.Down("d")),
        ListDir(
          List(
            FileEntry(4060174, "j"),
            FileEntry(8033020, "d.log"),
            FileEntry(5626152, "d.ext"),
            FileEntry(7214296, "k"),
          )
        ),
      )
    )

  }

  "Part 1" `should` "add up sizes of directories of sizes up 1000000" in {
    part1(input) should ===(95437L)
  }

  "Part 2" `should` "delete the smallest dir to reach enough free space" in {
    part2(input) should ===(24933642L)
  }
