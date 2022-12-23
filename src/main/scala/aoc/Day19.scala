package aoc

import cats.implicits.*
import Day19.Resource.*
import aoc.Collections.mapForKeys
import aoc.Functions.timed

import scala.collection.parallel.CollectionConverters.*
import scala.collection.mutable

object Day19:
  type Input = List[Blueprint]
  type Robot = Resource
  type Resources = Map[Resource, Int]

  enum Resource:
    case Ore, Clay, Obsidian, Geode

  case class Blueprint(id: Int, robotRequirements: Map[Robot, Resources]):
    val robotCosts: Map[Robot, Resources] =
      robotRequirements.view.mapValues(_.view.mapValues(-_).toMap).toMap

    val maxUsefulRobots: Map[Robot, Int] =
      robotRequirements
        .values
        .flatten
        .groupMapReduce(_._1)(_._2)(_ `max` _)
        .updated(Geode, Int.MaxValue)

    def feasibleRobots(resources: Resources) = robotRequirements.collect {
      case (robot, requirements) if requirements.forall {
             case (resource, amount) => resources.getOrElse(resource, 0) >= amount
           } =>
        robot
    }.toList

  object Blueprint:
    def parseAll(string: String): List[Blueprint] =
      string.linesIterator.map(parse).toList

    def parse(line: String): Blueprint =
      line match
        case s"Blueprint $id: Each ore robot costs $oreForOre ore. Each clay robot costs $oreForClay ore. Each obsidian robot costs $oreForObs ore and $clayForObs clay. Each geode robot costs $oreForGeode ore and $obsForGeode obsidian." =>
          Blueprint(
            id.toInt,
            Map(
              Ore -> Map(Ore -> oreForOre.toInt),
              Clay -> Map(Ore -> oreForClay.toInt),
              Obsidian -> Map(Ore -> oreForObs.toInt, Clay -> clayForObs.toInt),
              Geode -> Map(Ore -> oreForGeode.toInt, Obsidian -> obsForGeode.toInt),
            ),
          )
        case _ => ???

  def mostGeodes(minutes: Int, blueprint: Blueprint): Int =
    mostGeodes(minutes, resources = Map.empty, robots = Map(Ore -> 1), blueprint)

  def mostGeodes(
      minutes: Int,
      resources: Resources,
      robots: Map[Robot, Int],
      blueprint: Blueprint,
    ): Int =
    if minutes < 1 then resources.getOrElse(Geode, 0)
    else
      val feasibleRobots = blueprint.feasibleRobots(resources)
      val desiredRobots =
        if feasibleRobots.contains(Geode) then List(Geode)
        else if feasibleRobots.contains(Obsidian) then List(Obsidian)
        else
          feasibleRobots.filter(robot =>
            robots.getOrElse(robot, 0) < blueprint.maxUsefulRobots(robot)
          )

      val nextResources = resources |+| robots

      // Not building this minute
      val mostGeodesNotBuildingNow = mostGeodes(minutes - 1, nextResources, robots, blueprint)
      val mostGeodesBuilding = desiredRobots.map { robot =>
        mostGeodes(
          minutes - 1,
          nextResources |+| blueprint.robotCosts(robot),
          robots |+| Map(robot -> 1),
          blueprint,
        )
      }
      (mostGeodesNotBuildingNow :: mostGeodesBuilding).max

  def part1(input: Input): Int = input
    .par
    .map { blueprint =>
      val geodes = mostGeodes(minutes = 24, blueprint)
      println(s"Blueprint ${blueprint.id} can make $geodes geodes in 24 minutes")
      blueprint.id * geodes
    }
    .sum

  def part2(input: Input): Int = input
    .take(3)
    .par
    .map { blueprint =>
      mostGeodes(minutes = 32, blueprint)
    }
    .product

  def main(args: Array[String]): Unit =
    val input = Inputs(day = 19).parseLines(Blueprint.parse)
    println(timed(part1(input)))
    println(timed(part2(input))) // Takes too long to be practical
