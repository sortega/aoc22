package aoc

import scala.collection.mutable
import cats.implicits.*

object Day24:
  case class Valley(
      width: Int,
      height: Int,
      blizzards: Set[Blizzard],
    ):
    val period: Int = Math.lcm(width, height)
    lazy val allowedPos: Set[Pos] = (
      for
        row <- 1 to height
        col <- 1 to width
        pos = Pos(row, col) if !blizzards.exists(_.pos == pos)
      yield pos
    ).toSet + start + end

    def start: Pos = Pos(0, 1)
    def end: Pos = Pos(height + 1, width)

    def step: Valley = copy(blizzards = blizzards.map { blizzard =>
      blizzard.copy(pos =
        (blizzard.pos + blizzard.towards.pos).transform(
          row => Math.mod(row - 1, height) + 1,
          col => Math.mod(col - 1, width) + 1,
        )
      )
    })

    def toString(explorer: Pos): String =
      val wallChars = Map(
        Pos(0, 0) -> '#',
        Pos(0, width + 1) -> '#',
        Pos(height + 1, 0) -> '#',
        Pos(height + 1, width + 1) -> '#',
      )
      val blizzardChars = blizzards.groupBy(_.pos).map {
        case (pos, group) if group.size > 1 =>
          pos -> group.size.toString.head
        case (pos, single) => pos -> single.head.towards.char
      }
      require(!blizzardChars.contains(explorer))
      Matrix
        .fromMap(wallChars ++ blizzardChars.updated(explorer, 'E'), emptyValue = '.')
        .toString("")

  object Valley:
    def parse(string: String): Valley =
      val matrix = Matrix.parse(string)(char => CardinalPoint.fromChar(char))
      Valley(
        width = matrix.numCols - 2,
        height = matrix.numRows - 2,
        blizzards = matrix
          .toMap
          .collect {
            case (pos, Some(towards)) => Blizzard(pos, towards)
          }
          .toSet,
      )

  case class Blizzard(pos: Pos, towards: CardinalPoint)

  def fastestRoute(
      valleys: Vector[Valley],
      start: Pos,
      end: Pos,
      initialTime: Int,
    ): Int =
    case class State(pos: Pos, time: Int):
      def heuristic: Int = pos.manhattanDist(end)

      def nextStates: Set[State] =
        (pos.adjacent4 + pos)
          .intersect(valleys((time + 1) % valleys.size).allowedPos)
          .map(nextPos => State(nextPos, time + 1))

    val initial = State(start, time = initialTime)
    val gScore = mutable.Map(initial -> 0)
    val fScore = mutable.Map(initial -> initial.heuristic)
    val open = mutable.PriorityQueue(initial.heuristic -> initial)(Ordering.by(-_._1))
    val cameFrom = mutable.Map.empty[State, State]

    def reconstructPath(endState: State) =
      LazyList
        .unfold(endState) { state =>
          cameFrom.get(state).map(prevState => prevState.pos -> prevState)
        }
        .toList
        .reverse

    while open.nonEmpty do
      val (_, current) = open.dequeue()
      if current.pos == end then
//        for (pos, time) <- reconstructPath(current).zipWithIndex do
//          println(s"Minute $time:")
//          println(valleys(time).toString(pos))
//          println()
        return gScore(current)
      else
        for next <- current.nextStates
        do
          val tentativeGScore = gScore(current) + 1
          if tentativeGScore < gScore.getOrElse(next, Int.MaxValue) then
            cameFrom.update(next, current)
            gScore.update(next, tentativeGScore)
            fScore.update(next, tentativeGScore + next.heuristic)
            open.enqueue(fScore(next) -> next)

    throw new IllegalArgumentException("No path found")

  def part1(valley: Valley): Int =
    val valleys = Vector.iterate(valley, valley.period)(_.step)
    fastestRoute(valleys, valley.start, valley.end, initialTime = 0)

  def part2(valley: Valley): Int =
    val valleys = Vector.iterate(valley, valley.period)(_.step)
    val first = fastestRoute(valleys, valley.start, valley.end, initialTime = 0)
    val second = fastestRoute(valleys, valley.end, valley.start, initialTime = first)
    val third = fastestRoute(valleys, valley.start, valley.end, initialTime = first + second)
    first + second + third

  def main(args: Array[String]): Unit =
    val input = Valley.parse(Inputs(day = 24).slurp)
    println(part1(input))
    println(part2(input))
