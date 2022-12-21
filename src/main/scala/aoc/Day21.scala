package aoc

import cats.implicits.*

import concurrent.ExecutionContext.Implicits.global
import scala.collection.mutable
import scala.concurrent.duration.Duration
import scala.concurrent.{ Await, Promise }
import scala.util.Success

object Day21:
  type Input = Map[Name, Purpose]
  type Name = String

  enum Op:
    case Add, Sub, Mul, Div

  object Op:
    val parse = Map("+" -> Add, "-" -> Sub, "*" -> Mul, "/" -> Div)

  enum Purpose:
    case Operation(
        op: Op,
        left: Name,
        right: Name,
      )
    case Value(value: Long)

  object Purpose:
    def parse(input: String): Input =
      input
        .linesIterator
        .map {
          case s"$name: $child1 $op $child2" =>
            name -> Purpose.Operation(Op.parse(op), child1, child2)
          case s"$name: $number" => name -> Purpose.Value(number.toLong)
        }
        .toMap

  def lazyEvaluate(input: Input): Map[Name, Long] =
    val promises = input.map {
      case (name, Purpose.Value(value)) => name -> Promise.successful(value)
      case (name, Purpose.Operation(_, _, _)) => name -> Promise[Long]()
    }

    input.foreach {
      case (name, Purpose.Operation(op, left, right)) =>
        promises(left).future.zip(promises(right).future).onComplete {
          case Success((a, b)) =>
            promises(name).success(op match
              case Op.Add => a + b
              case Op.Sub => a - b
              case Op.Mul => a * b
              case Op.Div => a / b
            )
          case _ => ???
        }
      case _ => // Do nothing
    }

    promises.map {
      case (name, promise) => name -> Await.result(promise.future, Duration.Inf)
    }

  def part1(input: Input): Long = lazyEvaluate(input)("root")

  enum Expr:
    case Operation(
        op: Op,
        left: Expr,
        right: Expr,
      )
    case Value(value: Long)
    case Variable

    def hasVariable: Boolean = this match
      case Operation(_, left, right) => left.hasVariable || right.hasVariable
      case Variable => true
      case _ => false

  def part2(input: Input): Long =
    val values = lazyEvaluate(input)
    val usages: Map[Name, Set[Name]] = (for {
      case (name, Purpose.Operation(_, left, right)) <- input.toList
      usage <- List(left, right)
    } yield usage -> Set(name)).groupMapReduce(_._1)(_._2)(_.union(_))

    def buildEquation(name: Name, expr: Expr): (Expr, Long) =
      require(usages(name).size == 1)
      val usage = usages(name).head
      input(usage) match
        case Purpose.Operation(_, otherName, `name`) if usage == "root" =>
          (expr, values(otherName))
        case Purpose.Operation(_, `name`, otherName) if usage == "root" =>
          (expr, values(otherName))
        case Purpose.Operation(op, otherName, `name`) =>
          buildEquation(usage, Expr.Operation(op, Expr.Value(values(otherName)), expr))
        case Purpose.Operation(op, `name`, otherName) =>
          buildEquation(usage, Expr.Operation(op, expr, Expr.Value(values(otherName))))
        case _ => ???

    def solve(expr: Expr, value: Long): Long =
      expr match
        case Expr.Variable => value
        case Expr.Operation(op, left, Expr.Value(right)) =>
          op match
            case Op.Add => solve(left, value - right)
            case Op.Sub => solve(left, value + right)
            case Op.Mul => solve(left, value / right)
            case Op.Div => solve(left, value * right)
        case Expr.Operation(op, Expr.Value(left), right) =>
          op match
            case Op.Add => solve(right, value - left)
            case Op.Sub => solve(right, left - value)
            case Op.Mul => solve(right, value / left)
            case Op.Div => solve(right, left / value)
        case _ => ???

    val (expr, value) = buildEquation("humn", Expr.Variable)
    solve(expr, value)

  def main(args: Array[String]): Unit =
    val input = Purpose.parse(Inputs(day = 21).slurp)
    println(part1(input))
    println(part2(input))
