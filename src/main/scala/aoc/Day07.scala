package aoc

import atto.*
import atto.Atto.*
import cats.*
import cats.data.*
import cats.implicits.*

object Day07:
  type Listing = List[Command]

  object Listing:
    val parser: Parser[Listing] = sepBy(Command.parser, char('\n'))

  enum Command:
    case ChangeDir(target: Target)
    case ListDir(output: List[ListEntry])

  object Command:
    val cdParser: Parser[ChangeDir] = string("cd ") ~> Target.parser.map(ChangeDir.apply)
    val lsParser: Parser[ListDir] =
      string("ls") ~> many(char('\n') ~> ListEntry.parser).map(ListDir.apply)
    val parser: Parser[Command] = string("$ ") ~> (cdParser | lsParser)

  enum Target:
    case Root
    case Up
    case Down(dir: String)

  object Target:
    val parser: Parser[Target] =
      string("/").as(Target.Root) |
        string("..").as(Target.Up) |
        name.map(Target.Down.apply)

  enum ListEntry:
    case DirEntry(name: String)
    case FileEntry(size: Long, name: String)

  object ListEntry:
    val parser: Parser[ListEntry] =
      (string("dir ") ~> name.map(DirEntry.apply)) |
        (long <~ skipWhitespace, name).mapN(FileEntry.apply)

  val name: Parser[String] = stringOf(letter | char('.')).namedOpaque("id")

  enum FileTree:
    case File(size: Long)
    case Dir(contents: Map[String, FileTree] = Map.empty)

    def nest(parents: List[String]): FileTree = parents.foldLeft(this)(_.nest(_))
    def nest(parent: String): Dir = Dir(Map(parent -> this))

    def combine(other: FileTree): FileTree = (this, other) match
      case (Dir(left), Dir(right)) =>
        val contents: List[(String, FileTree)] = left.toList ++ right.toList
        Dir(contents.groupMapReduce(_._1)(_._2)(_.combine(_)))

      case _ => throw new UnsupportedOperationException(s"Cannot merge $this with $other")

    def totalSize: Long = this match
      case File(size) => size
      case Dir(entries) => entries.values.map(_.totalSize).sum

    def dirSizes: Map[String, Long] = this match
      case Dir(contents) =>
        Map("" -> totalSize) ++ contents.toList.foldMap { (name, tree) =>
          tree.dirSizes.map((k, v) => s"$name/$k" -> v)
        }
      case _ => Map.empty

  object FileTree:
    type Path = List[String]

    def from(listing: Listing): FileTree =
      type WritingTrees[A] = Writer[FileTree, A]
      type PathState[F[_], A] = StateT[F, Path, A]
      listing
        .traverse_[PathState[WritingTrees, *], Unit] {
          case Command.ChangeDir(Target.Root) => StateT.set(Nil)
          case Command.ChangeDir(Target.Up) => StateT.modify(_.tail)
          case Command.ChangeDir(Target.Down(dir)) => StateT.modify(dir :: _)
          case Command.ListDir(entries) =>
            StateT.inspectF { currentPath =>
              FileTree.from(entries).nest(currentPath).tell
            }
        }
        .runA(Nil)
        .written

    def from(entries: List[ListEntry]): Dir =
      Dir(entries.map {
        case ListEntry.FileEntry(size, name) => name -> File(size)
        case ListEntry.DirEntry(name) => name -> Dir()
      }.toMap)

    implicit val fileTreeMonoid: Monoid[FileTree] = new Monoid[FileTree]:
      override def empty: FileTree = Dir()
      override def combine(l: FileTree, r: FileTree): FileTree = l.combine(r)

  def part1(input: Listing): Long =
    val dirSizes = FileTree.from(input).dirSizes
    dirSizes.values.toList.foldMap { size =>
      if (size <= 100000L) size else 0
    }

  def part2(input: Listing): Long =
    val tree = FileTree.from(input)
    val free = 70000000 - tree.totalSize
    val toRelease = 30000000 - free
    tree.dirSizes.values.toList.filter(_ >= toRelease).min

  def main(args: Array[String]): Unit =
    val input = Inputs(day = 7).parse(Listing.parser)
    println(part1(input))
    println(part2(input))
