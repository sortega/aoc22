package aoc

import atto.*
import atto.Atto.*
import cats.*
import cats.data.*
import cats.implicits.*
import Collections.mapForKeys

import scala.collection.mutable
import scala.util.chaining.*

object Day12:
  case class Input(
      heightMap: Map[Pos, Char],
      start: Pos,
      goal: Pos,
    )

  object Input:
    def parse(string: String): Input =
      val charMap = Matrix(string.linesIterator.map(_.toVector).toVector).toMap
      Input(
        heightMap = charMap.map {
          case (k, 'S') => k -> 'a'
          case (k, 'E') => k -> 'z'
          case other => other
        },
        start = charMap.collectFirst {
          case (pos, 'S') => pos
        }.get,
        goal = charMap.collectFirst {
          case (pos, 'E') => pos
        }.get,
      )

  case class Graph(edges: Map[Pos, Set[Pos]]):
    def invert: Graph = Graph(
      (for {
        (source, targets) <- edges.toList
        target <- targets
      } yield target -> source).groupMapReduce(_._1)(tuple => Set(tuple._2))(_ `union` _)
    )

    def distancesFrom(start: Pos): Map[Pos, Int] =
      val unvisited = edges.keySet.to(mutable.Set)
      val distances = mutable.Map(start -> 0).withDefaultValue(Int.MaxValue)
      val queue = mutable.PriorityQueue.empty(Ordering.by[Pos, Int](pos => -distances(pos)))
      queue.addOne(start)

      while (queue.nonEmpty)
        val current = queue.dequeue()
        if (unvisited(current) && distances(current) < Int.MaxValue)
          val oneFromCurrent = distances(current) + 1
          for (neighbor <- edges(current).filter(unvisited))
            if (oneFromCurrent < distances(neighbor))
              distances.update(neighbor, oneFromCurrent)
              queue.addOne(neighbor)
          unvisited.remove(current)

      distances.toMap.withDefaultValue(Int.MaxValue)

  object Graph:
    def fromHeights(heightMap: Map[Pos, Char]): Graph =
      val vertices = heightMap.keySet
      Graph(mapForKeys(vertices) { vertex =>
        val height = heightMap(vertex)
        vertex.adjacent4.filter { adjacentVertex =>
          heightMap.get(adjacentVertex).exists(_ - height <= 1)
        }
      })

  def part1(input: Input): Int =
    Graph.fromHeights(input.heightMap).distancesFrom(input.start)(input.goal)

  def part2(input: Input): Int =
    val distances = Graph.fromHeights(input.heightMap).invert.distancesFrom(input.goal)
    input
      .heightMap
      .collect {
        case (pos, 'a') => distances(pos)
      }
      .min

  def main(args: Array[String]): Unit =
    val input = Input.parse(Inputs(day = 12).slurp)
    println(part1(input))
    println(part2(input))
