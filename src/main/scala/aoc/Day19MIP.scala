package aoc

import aoc.Collections.mapForKeys
import aoc.Day19MIP.Resource.*
import aoc.Functions.timed
import cats.implicits.*
import com.google.ortools.Loader
import com.google.ortools.linearsolver.*

import scala.util.chaining.*
import scala.collection.mutable
import scala.collection.parallel.CollectionConverters.*

object Day19MIP:
  type Input = List[Blueprint]
  type Robot = Resource
  type Resources = Map[Resource, Int]

  private def pluralize(word: String, amount: Int): String =
    if amount == 1 then word else word + "s"

  enum Resource:
    case Ore, Clay, Obsidian, Geode

    def robotName: String = s"$this-${robotAction}ing robot"

    def robotAction: String = this match
      case Ore | Clay | Obsidian => "collect"
      case Geode => "crack"

    def pluralize(amount: Int): String = this match
      case Geode => Day19MIP.pluralize(toString, amount)
      case _ => toString // Uncountable names

    override def toString: String = productPrefix.toLowerCase.nn

  private def perResource[A](f: Resource => A): Map[Resource, A] =
    Resource.values.map(r => r -> f(r)).toMap

  case class Blueprint(id: Int, robotRequirements: Map[Robot, Resources])

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

  case class Plan(
      buildSchedule: List[Option[Resource]],
      score: Int,
      timeLimit: Int,
      blueprint: Blueprint,
    ):
    def narrate: String =
      def a(word: String): String =
        s"${(if "aeiou".contains(Character.toLowerCase(word.head)) then "an" else "a")} $word"

      val buffer = mutable.StringBuilder()
      val resources = mutable.Map.empty[Resource, Int].withDefaultValue(0)
      val robots = mutable.Map[Resource, Int](Ore -> 1).withDefaultValue(0)
      (0 until timeLimit).foreach { index =>
        buffer.append(s"== Minute ${index + 1} ==\n")
        buildSchedule(index).foreach { robot =>
          val requirements = blueprint.robotRequirements(robot)
          requirements.foreach { (resource, amount) =>
            resources.update(resource, resources(resource) - amount)
          }
          buffer.append(
            s"Spend ${formatResources(requirements)} to start building ${a(robot.robotName)}.\n"
          )
        }
        Resource.values.foreach { resource =>
          val amount = robots(resource)
          if amount > 0 then
            resources.update(resource, resources(resource) + amount)
            buffer.append(
              "%d %s %s %d %s; you now have %s.\n".format(
                amount,
                pluralize(resource.robotName, amount),
                resource.robotAction + (if amount > 1 then "" else "s"),
                amount,
                resource.pluralize(amount),
                formatResource(resource, resources(resource)),
              )
            )
        }
        buildSchedule(index).foreach { robot =>
          robots.update(robot, robots(robot) + 1)
          buffer.append(
            s"The new ${robot.robotName} is ready; you now have ${robots(robot)} of them.\n"
          )
        }
        buffer.append("\n")
      }
      buffer.toString

    private def formatResource(resource: Resource, count: Int): String = resource match
      case Ore | Clay | Obsidian => s"$count ${resource.pluralize(count)}"
      case Geode => s"$count open ${resource.pluralize(count)}"

    private def formatResources(resources: Resources): String =
      resources
        .toList
        .sortBy(_._1.ordinal)
        .map {
          case (resource, count) => formatResource(resource, count)
        }
        .mkString(" and ")

    override def toString: String =
      "Plan(score=%d, buildSchedule=%s)".format(
        score,
        buildSchedule
          .zipWithIndex
          .collect {
            case (Some(robot), index) => s"$index:$robot"
          }
          .mkString(", "),
      )

  object Plan:
    def findBestFor(blueprint: Blueprint, timeLimit: Int): Plan =
      val solver = MPSolver.createSolver("SCIP").nn

      case class StepVars(
          minute: Int,
          buildRobot: Map[Robot, MPVariable],
          resourcesAfterBuild: Map[Resource, MPVariable],
          finalResources: Map[Resource, MPVariable],
          finalRobots: Map[Robot, MPVariable],
        )

      object StepVars:
        def first: StepVars = StepVars(
          minute = 1,
          buildRobot =
            perResource(resource => solver.makeIntVar(0, 0, s"build $resource at minute 1").nn),
          resourcesAfterBuild = perResource { resource =>
            val value = if resource == Ore then 1 else 0
            solver.makeIntVar(value, value, s"$resource after build at minute 1").nn
          },
          finalResources = perResource { resource =>
            val value = if resource == Ore then 1 else 0
            solver.makeIntVar(value, value, s"final $resource at minute 1").nn
          },
          finalRobots = perResource { resource =>
            val value = if resource == Ore then 1 else 0
            solver.makeIntVar(value, value, s"final $resource robots at minute 1").nn
          },
        )

        def after(prevStep: StepVars): StepVars =
          val minute = prevStep.minute + 1

          val buildRobot = perResource { resource =>
            solver.makeBoolVar(s"build $resource at minute $minute").nn
          }
          solver.makeConstraint(0, 1, "build at most one robot").nn.tap { constraint =>
            buildRobot.values.foreach(buildVar => constraint.setCoefficient(buildVar, 1))
          }

          val resourcesAfterBuild = perResource { resource =>
            val resourceVar =
              solver.makeIntVar(0, Int.MaxValue, s"$resource after build at minute $minute").nn
            val constraint = solver.makeConstraint(0, 0).nn
            val initialAmount = prevStep.finalResources(resource)
            constraint.setCoefficient(initialAmount, 1)
            constraint.setCoefficient(resourceVar, -1)
            for {
              (robot, requirements) <- blueprint.robotRequirements
              amount <- requirements.get(resource)
            } constraint.setCoefficient(buildRobot(robot), -amount)
            resourceVar
          }

          val finalResources = perResource { resource =>
            val resourceVar =
              solver.makeIntVar(0, Int.MaxValue, s"final $resource at minute $minute").nn
            val constraint = solver.makeConstraint(0, 0).nn
            constraint.setCoefficient(resourceVar, 1)
            constraint.setCoefficient(resourcesAfterBuild(resource), -1)
            constraint.setCoefficient(prevStep.finalRobots(resource), -1)
            resourceVar
          }

          val finalRobots = perResource { resource =>
            val robotVar =
              solver.makeIntVar(0, Int.MaxValue, s"final $resource robots at minute $minute").nn
            val constraint = solver.makeConstraint(0, 0).nn
            constraint.setCoefficient(robotVar, 1)
            constraint.setCoefficient(prevStep.finalRobots(resource), -1)
            constraint.setCoefficient(buildRobot(resource), -1)
            robotVar
          }

          StepVars(minute, buildRobot, resourcesAfterBuild, finalResources, finalRobots)

      val steps = List.iterate(StepVars.first, timeLimit)(StepVars.after)

      val objective = solver.objective.nn
      objective.setCoefficient(steps.last.finalResources(Geode), 1)
      objective.setMaximization()

      val result = solver.solve()
      require(result == MPSolver.ResultStatus.OPTIMAL)
      val buildSchedule = steps.map { stepVars =>
        stepVars.buildRobot.collectFirst {
          case (robot, variable) if variable.solutionValue() == 1d => robot
        }
      }
      Plan(buildSchedule, score = math.round(objective.value: Double).toInt, timeLimit, blueprint)

  def part1(input: Input): Int =
    input
      .par
      .map { blueprint =>
        val plan = Plan.findBestFor(blueprint, timeLimit = 24)
//      println(plan.narrate)
        blueprint.id * plan.score
      }
      .sum

  def part2(input: Input): Int =
    input
      .take(3)
      .par
      .map { blueprint =>
        Plan.findBestFor(blueprint, timeLimit = 32).score
      }
      .product

  def main(args: Array[String]): Unit =
    Loader.loadNativeLibraries()
    val input = Inputs(day = 19).parseLines(Blueprint.parse)
    println(timed(part1(input))) // 1306
    println(timed(part2(input)))
