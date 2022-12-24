package aoc

import aoc.Functions.timed
import cats.implicits.*

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.chaining.*

object Day16:
  type Input = List[Valve]
  type Id = String

  case class Valve(
      id: String,
      rate: Int,
      connections: Set[Id],
    )

  object Valve:
    def parse(line: String): Valve = line match
      case s"Valve $id has flow rate=$rate; tunnel$s lead$s2 to valve$s3 $connections" =>
        Valve(id, rate.toInt, connections.split(", ").nn.map(_.nn).toSet)

  private def floydWarshallDistances(input: Input): Map[(Id, Id), Int] =
    val graph = (for {
      valve <- input
      connection <- valve.connections
    } yield (valve.id, connection)).toSet
    floydWarshallDistances(graph)

  private def floydWarshallDistances(graph: Set[(Id, Id)]): Map[(Id, Id), Int] =
    val vertices: Set[Id] = graph.flatMap { case (a, b) => Set(a, b) }
    var dist: Map[(Id, Id), Int] =
      (graph.map(edge => edge -> 1) ++ vertices.map(v => (v, v) -> 0)).toMap
    for {
      intermediate <- vertices
      from <- vertices
      to <- vertices
      edge = (from, to)
      step1 <- dist.get((from, intermediate))
      step2 <- dist.get((intermediate, to))
      candidateDist = step1 + step2
      if dist.get(edge).forall(_ > candidateDist)
    } dist = dist.updated(edge, candidateDist)
    dist

  def part1(input: Input): Int =
    val maxMinutes = 30
    val dist: Map[(Id, Id), Int] = floydWarshallDistances(input)
    val valves: Map[Id, Valve] = input.map(valve => valve.id -> valve).toMap
    val eligibleValves: Set[Id] = input.collect {
      case valve if valve.rate > 0 => valve.id
    }.toSet

    case class State(
        pos: Id = "AA",
        minutes: Int = 0,
        rate: Int = 0,
        score: Int = 0,
        opened: Set[Id] = Set.empty,
        actions: Vector[String] = Vector.empty,
      ):

      override def toString =
        s"State(minute=$minutes, at=$pos, score=$score, bound=$bound, open=${opened.toList.sorted.mkString(",")})"

      val bound: Int =
        val sortedValves = eligibleValves.diff(opened).toList.sortBy(id => -valves(id).rate)
        score + remainingMinutes * rate + (for
          (valve, index) <- sortedValves.zipWithIndex
          timeOpen = remainingMinutes - dist(pos -> valve) - 1 - index
          if timeOpen > 0
        yield valves(valve).rate * timeOpen).sum

      val priority: Int = score + remainingMinutes * rate

      def remainingMinutes: Int = maxMinutes - minutes

      def isComplete: Boolean = remainingMinutes <= 0

      def nextStates: List[State] =
        // Do nothing until the end
        tick(remainingMinutes).copy(actions = actions :+ s"wait_$remainingMinutes") ::
          // Open a valve
          (if eligibleValves(pos) && !opened(pos) then List(tick().open) else Nil) :::
          // Move to another closed valve
          valves(pos).connections.toList.map(nextPos => tick().move(nextPos))

      def tick(n: Int = 1): State = copy(minutes = minutes + n, score = score + n * rate)
      def open: State = copy(
        opened = opened + pos,
        rate = rate + valves(pos).rate,
        actions = actions :+ s"open_$pos",
      )
      def move(toPos: Id): State = copy(pos = toPos, actions = actions :+ s"goto_$toPos")

    val initial = State()

    var best = none[State]
    val visited = mutable.Set.empty[State]
    var numVisited = 0
    var numPruned = 0
    val queue = mutable.PriorityQueue(initial)(Ordering.by(_.priority))
    while queue.nonEmpty do
      val state = queue.dequeue()
      numVisited += 1
      visited.add(state)
      if numVisited % 100000 == 0 then
        println(
          f"Visited $numVisited states, $numPruned pruned, ${queue.size} queued, best = $best, examining $state"
        )
      if state.isComplete && best.forall(_.score < state.score) then
        println(s"New best: $state")
        best = Some(state)
      if !state.isComplete then
        val candidates = state.nextStates
        val filteredCandidates = candidates
          .filter(state => !visited.contains(state) && best.forall(_.score < state.bound))
        numPruned += candidates.size - filteredCandidates.size
        queue.addAll(filteredCandidates)

    println(best)
    best.get.actions.foreach(println)
    best.get.score

  def part2Old(input: Input): Int =
    val valves = input.map(valve => valve.id -> valve).toMap

    case class State(
        man: Id = "AA",
        elephant: Id = "AA",
        minutes: Int = 0,
        rate: Int = 0,
        score: Int = 0,
        opened: Set[Id] = Set.empty,
        actions: Vector[String] = Vector.empty,
      ):

      val bound: Int =
        val rates = input.filter(valve => !opened.contains(valve.id)).map(_.rate).sortBy(-_)

        @tailrec
        def loop(
            minutes: Int,
            rates: List[Int],
            rate: Int,
            pressure: Int,
          ): Int =
          if minutes == 26 then pressure
          else
            rates match
              case nextRate :: nextRate2 :: otherRates if minutes <= 25 =>
                loop(minutes + 1, otherRates, rate + nextRate + nextRate2, pressure + rate)
              case nextRate :: otherRates if minutes <= 25 =>
                loop(minutes + 1, otherRates, rate + nextRate, pressure + rate)
              case _ => pressure + (26 - minutes) * rate

        loop(minutes, rates, rate, score)

      def remainingMinutes: Int = 26 - minutes

      def isComplete: Boolean = remainingMinutes <= 0

      def nextStates: List[State] =
        // Do nothing until the end
        tick(remainingMinutes).copy(actions = actions :+ s"wait $remainingMinutes") :: (
          for {
            manAction <- valves(man).connections.toList ++ (
              if opened(man) || valves(man).rate == 0 then None
              else Some("open")
            )
            state2 = manAction match
              case "open" => open(man)
              case pos => copy(man = pos)
            elephantAction <- valves(elephant).connections.toList ++ (
              if opened(elephant) || valves(man).rate == 0 then None
              else Some("open")
            )
            if !(manAction == "open" && elephantAction == "open" && man == elephant)
            state3 = manAction match
              case "open" => state2.open(elephant)
              case pos => copy(elephant = pos)
          } yield state3.tick().copy(actions = actions :+ s"man:$manAction,ele:$elephantAction")
        )

      def tick(n: Int = 1): State = copy(minutes = minutes + n, score = score + n * rate)

      def open(id: Id): State = copy(opened = opened + id, rate = rate + valves(id).rate)

    val initial = State()

    var best = none[State]
    var numVisited = 0
    //    val queue = mutable.PriorityQueue(initial)(
    //      Ordering.by(state => state.score + state.rate * state.remainingMinutes)
    //    )
    val queue = mutable.PriorityQueue(initial)(Ordering.by(_.bound))
    //    val queue = mutable.Queue(initial)
    while queue.nonEmpty do
      val state = queue.dequeue()
      numVisited += 1
      if numVisited % 10000 == 0 then
        println(s"Visited $numVisited states, ${queue.size} queued, best = $best")
        if state.isComplete && best.forall(_.score < state.score) then
          println(s"New best: $state")
          best = Some(state)
      if !state.isComplete then
        val candidates = state.nextStates
        val filteredCandidates = candidates
          .filter(state => best.forall(_.score < state.bound))
        queue.addAll(filteredCandidates)

    println(best)
    best.get.score

  def part2(input: Input): Int =
    val maxMinutes = 26
    val dist: Map[(Id, Id), Int] = floydWarshallDistances(input)
    val valves: Map[Id, Valve] = input.map(valve => valve.id -> valve).toMap
    val eligibleValves: Set[Id] = input.collect {
      case valve if valve.rate > 0 => valve.id
    }.toSet

    enum Action:
      case Wait
      case Open(id: Id)
      case Move(to: Id)

    case class State(
        pos: List[Id] = List("AA", "AA"),
        minutes: Int = 0,
        rate: Int = 0,
        score: Int = 0,
        opened: Set[Id] = Set.empty,
      )(
        val actions: Vector[(Action, Action)] = Vector.empty
      ):
      require(pos.size == 2)

      override def toString =
        s"State(minute=$minutes, at=[${pos.mkString(",")}], score=$score, bound=$bound, open=${opened.toList.sorted.mkString(",")})"

      val bound: Int =
        val sortedValves = eligibleValves.diff(opened).toList.sortBy(id => -valves(id).rate)
        score + remainingMinutes * rate + (for
          (valve, index) <- sortedValves.zipWithIndex
          timeOpen = remainingMinutes - (dist(pos(0) -> valve) `min` dist(pos(1) -> valve)) - 1 - (index / 2)
          if timeOpen > 0
        yield valves(valve).rate * timeOpen).sum

      val priority: Int = score + remainingMinutes * rate

      def remainingMinutes: Int = maxMinutes - minutes

      def isComplete: Boolean = remainingMinutes <= 0

      def nextStates: List[State] =
        if eligibleValves.diff(opened).isEmpty then
          List(tick(remainingMinutes).copy()(actions = actions :+ (Action.Wait, Action.Wait)))
        else
          val List(me, assistant) = pos
          for
            myAction <- actionsFrom(me)
            afterMyAction = myAction match
              case Action.Wait => tick()
              case Action.Open(valve) => tick().open(valve)
              case Action.Move(to) => tick().move(me, to)
            itsAction <- afterMyAction.actionsFrom(assistant)
          yield (itsAction match
            case Action.Wait => afterMyAction
            case Action.Open(valve) => afterMyAction.open(valve)
            case Action.Move(to) => afterMyAction.move(assistant, to)
          ).copy()(actions = actions :+ (myAction, itsAction))

      private def actionsFrom(pos: Id): List[Action] =
        (if eligibleValves(pos) && !opened(pos) then List(Action.Open(pos)) else Nil) :::
          valves(pos).connections.map(to => Action.Move(to)).toList

      def tick(n: Int = 1): State = copy(minutes = minutes + n, score = score + n * rate)(actions)

      def open(id: Id): State =
        if opened(id) then this
        else copy(opened = opened + id, rate = rate + valves(id).rate)(actions)

      def move(from: Id, to: Id): State =
        copy(pos = pos.updated(pos.indexOf(from), to).sorted)(actions)

    val initial = State()()

    var best = none[State]
    val visited = mutable.Set.empty[State]
    var numVisited = 0
    var numPruned = 0
    val queue = mutable.PriorityQueue(initial)(Ordering.by(_.priority))
    while queue.nonEmpty do
      val state = queue.dequeue()
      numVisited += 1
      visited.add(state)
      if numVisited % 100000 == 0 then
        println(
          f"Visited $numVisited states, $numPruned pruned, ${queue.size} queued, best = $best, examining $state"
        )
      if state.isComplete && best.forall(_.score < state.score) then
        println(s"New best: $state")
        best = Some(state)
      if !state.isComplete then
        val candidates = state.nextStates
        val filteredCandidates = candidates
          .filter(state => !visited.contains(state) && best.forall(_.score < state.bound))
        numPruned += candidates.size - filteredCandidates.size
        queue.addAll(filteredCandidates)

    println(best)
    best.get.score

  def main(args: Array[String]): Unit =
    val input = Inputs(day = 16).parseLines(Valve.parse)

    println(timed(part1(input)))
    println(timed(part2(input)))
