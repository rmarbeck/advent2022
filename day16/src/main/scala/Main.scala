import scala.io.Source
import com.typesafe.scalalogging.Logger

import scala.annotation.tailrec
import scala.util.Sorting
import Dijkstra.*

import scala.collection.mutable

val loggerAOC = Logger("aoc")
val loggerAOCPart1 = Logger("aoc.part1")
val loggerAOCPart2 = Logger("aoc.part2")

@main def hello: Unit =
  loggerAOC.trace("Root trace activated")
  loggerAOC.debug("Root debug activated")
  println("Launching Day16")
  List[() => (String, String)]( () => Solver.solveTest, () => Solver.solve).foreach: f =>
    val (score1, score2) = f.apply()
    println(s"1 : ${score1}")
    println(s"2 : ${score2}")
    println(s"----------------")
  println("Done")

var round = 0

object Solver:
  def runOn(inputLines: Seq[String]): (String, String) =

    val maze = Maze(
      inputLines.map:
        case s"Valve $name has flow rate=$flow; tunnel$_ lead$_ to valve$_ $valves" => Valve(name, flow.toInt, valves.split(", ").toSeq)
    )

    given ValveGraph = ValveGraph(maze)

    val start = PartialResult(30, "AA", maze, 0, Nil)
    val start2 = PartialResultPart2((26, 26), ("AA", "AA"), maze, 0, Nil)
    loggerAOCPart2.whenDebugEnabled {
      round = 0
    }

    val result1 = s"${solveMaze(List(start), None)}"
    val result2 = s"${solveMaze2(List(start2), None)}"

    (s"${result1}", s"${result2}")

  def solveTest: (String, String) =
    solver("test.txt")
  def solve: (String, String) =
    solver("data.txt")
  private def solver(fileName: String): (String, String) =
    val bufferedSource = Source.fromResource(fileName)
    val lines = bufferedSource.getLines().toSeq
    lines match
      case Nil => ("", "")
      case _ => runOn(lines)
end Solver

object Proxy:
  private val cache: mutable.HashMap[String, Int] = mutable.HashMap[String, Int]()
  def get(valveFrom: Valve, valveTo: Valve)(using graph: ValveGraph): Int =
    cache.getOrElseUpdate(valveFrom.name+valveTo.name, Dijkstra.solve(graph, valveFrom, List(valveTo)))

class ValveGraph(val maze: Maze) extends Graph[Valve]:
  override val getElements: Seq[Valve] = maze.valves
  override def weightBetween(first: Data[Valve], second: Data[Valve]): Long = 1l
  override def getNeighboursOfIn(current: Data[Valve], potentialNeighbours: Seq[Data[Valve]]): Seq[Data[Valve]] =
    potentialNeighbours.filter: potentialNeighbour =>
      current.getElement.valveNames.contains(potentialNeighbour.getElement.name)

case class Maze(valves: Seq[Valve]):
  lazy val value = valves.map(_.pressure).sum
  def get(name: String): Valve = valves.find(_.name == name) match
    case Some(valve) => valve
    case None => throw Exception(s"Not supported for $name")
  def openValve(valve: Valve): Maze =
    Maze(
      valves.map:
        case value if value == valve => valve.copy(pressure = 0)
        case value => value
    )

case class Valve(name: String, pressure: Int, valveNames: Seq[String])

case class PartialResultPart2(remainingSteps: (Int, Int), start: (String, String), maze: Maze, currentPressure: Int, openedValves: List[String])(using graph: ValveGraph) extends Ordered[PartialResultPart2]:
  def canonical: PartialResultPart2 =
    maze.value == 0 match
      case true => this.copy(remainingSteps = (0, 0))
      case false => this
  /*override def equals(o: Any): Boolean = o match
    case toCompare @ PartialResultPart2(_, _, _, _, _) => this.raw == toCompare.raw
    case _ => false

  lazy val raw = s"${remainingSteps._1}.${remainingSteps._2}.${start._1}.${start._2}.${currentPressure}.${openedValves.mkString(".")}.${maze.valves.map(current => s"${current.name}.${current.pressure}").mkString(".")}"*/

  override def compare(that: PartialResultPart2): Int =
    def mazePressure(pr: PartialResultPart2) = List(pr.start._1, pr.start._2).map(pr.maze.get(_).pressure).max
    currentPressure.compare(that.currentPressure) match
      case 0 => this.maxPotential.compare(that.maxPotential) match
        case 0 => mazePressure(this).compare(mazePressure(that))
        case value => value
      case value => value

  lazy val simple: String = s"$start <-> $currentPressure <-> $openedValves <-> $remainingSteps <+> $maxPotential ($currentPressure - ${maze.value*(remainingSteps._1+remainingSteps._2)})"
  lazy val startToUse = (remainingSteps._1, remainingSteps._2) match
    case (value1, value2) if value1 >= value2 => start._1
    case _ => start._2
  lazy val steps = math.max(remainingSteps._1, remainingSteps._2)
  lazy val maxPotential: Int =
    currentPressure + maxPotentialFrom2((maze.get(start._1), maze.get(start._2)), steps)
  lazy val isASolution: Boolean = remainingSteps == (0,0)

  def distancesFrom(valve: Valve, distanceMax: Int) =
    maze.valves.filterNot(_.pressure == 0).map: currentValve =>
      (currentValve, Proxy.get(valve, currentValve))
    .filter(_._2 < distanceMax)

  def maxPotentialFrom(starts: (Valve, Valve), distanceMax: Int) =
    maze.valves.filterNot(_.pressure == 0).zipWithIndex.map:
      case (currentValve, index) => currentValve.pressure * math.max(distanceMax - (index/2)*2 - math.min(Proxy.get(starts._1, currentValve), Proxy.get(starts._2, currentValve)), 0)
    .sum

  def maxPotentialFrom2(starts: (Valve, Valve), distanceMax: Int) =
    maze.valves.filterNot(_.pressure == 0).map:
      case currentValve => (currentValve.pressure, math.max(distanceMax - math.min(Proxy.get(starts._1, currentValve), Proxy.get(starts._2, currentValve)), 0))
    .sortBy(current => current._1 * (current._2 - 1)).reverse.zipWithIndex.map:
      case value @ ((pressure, range), index) =>
        //println(s"$value")
        pressure * math.max(range - 1 - 2*(index/2),0)
    .sum

  def distancesFrom(starts: (Valve, Valve), distanceMax: Int) =
    maze.valves.filterNot(_.pressure == 0).map: currentValve =>
      (currentValve, math.min(Proxy.get(starts._1, currentValve), Proxy.get(starts._2, currentValve)))
    .filter(_._2 < distanceMax)

  lazy val next: List[PartialResultPart2] =
    def newSteps(reducingDistance: Int): (Int, Int) =
      remainingSteps match
        case (value1, value2) if value1 >= value2 => (value1 - reducingDistance, value2)
        case (value1, value2) => (value1, value2  - reducingDistance)

    def newValveNames(valveName: String): (String, String) =
      remainingSteps match
        case (value1, value2) if value1 >= value2 => (valveName, start._2)
        case (value1, value2) => (start._1, valveName)
    isASolution match
      case true => Nil
      case _ =>
        val headValve = maze.get(startToUse)
        headValve.pressure match
          case value if value != 0 && steps >= 2 =>
            distancesFrom(headValve, steps).map:
              case (valve, distance) => PartialResultPart2(newSteps(distance + 1), newValveNames(valve.name), maze.openValve(valve), currentPressure + (valve.pressure * (steps - distance - 1)), valve.name +: openedValves).canonical
            .toList
          case _ =>
            headValve.valveNames.map: currentValveName =>
              PartialResultPart2(newSteps(1), newValveNames(currentValveName), maze, currentPressure, openedValves).canonical
            .toList


case class PartialResult(remainingSteps: Int, start: String, maze: Maze, currentPressure: Int, openedValves: List[String])(using graph: ValveGraph) extends Ordered[PartialResult]:
  override def compare(that: PartialResult): Int =
    def mazePressure(pr: PartialResult) = pr.maze.get(pr.start).pressure
    currentPressure.compare(that.currentPressure) match
      case 0 => this.maxPotential.compare(that.maxPotential) match
        case 0 => mazePressure(this).compare(mazePressure(that))
        case value => value
      case value => value
  lazy val simple: String = s"$start <-> $currentPressure <-> $openedValves <-> $remainingSteps <+> $maxPotential"
  lazy val maxPotential: Int =
    currentPressure + distancesFrom(maze.get(start), remainingSteps - 1).zipWithIndex.map:
      case ((valve, distance), index) =>  valve.pressure * math.max(remainingSteps - (distance + index), 0)
    .sum
  lazy val isASolution: Boolean = remainingSteps == 0

  def distancesFrom(valve: Valve, distanceMax: Int) =
    maze.valves.filterNot(_.pressure == 0).map: currentValve =>
      (currentValve, Proxy.get(valve, currentValve))
    .filter(_._2 < distanceMax)
  lazy val next: List[PartialResult] =
    isASolution match
      case true => Nil
      case _ =>
        val headValve = maze.get(start)
        headValve.pressure match
          case value if value != 0 && remainingSteps >= 2 =>
            distancesFrom(headValve, remainingSteps).map:
              case (valve, distance) => PartialResult(remainingSteps - distance - 1, valve.name, maze.openValve(valve), currentPressure + (valve.pressure * (remainingSteps - distance - 1)), valve.name +: openedValves)
            .toList
          case _ =>
            headValve.valveNames.map: currentValveName =>
              PartialResult(remainingSteps - 1, currentValveName, maze, currentPressure, openedValves)
            .toList


@tailrec
def solveMaze(currentPResults: List[PartialResult], bestSolutionFound: Option[PartialResult])(using graph: ValveGraph): Int =
  currentPResults match
    case Nil => bestSolutionFound match
      case Some(value) => value.currentPressure
      case None => throw Exception("Not Found")
    case head :: tail =>
      loggerAOCPart1.trace(s"IN --> ${head.simple}")
      val newPartialResults = head.next
      loggerAOCPart1.whenTraceEnabled {
        newPartialResults.foreach: current =>
          loggerAOCPart1.trace(current.simple)
      }
      val newBestSolution = (bestSolutionFound +: newPartialResults.filter(_.isASolution).map(Some(_))).flatten.sorted.lastOption
      loggerAOCPart1.whenTraceEnabled {
        loggerAOCPart1.trace(s"----------> ${newPartialResults.filter(_.isASolution).map(Some(_)).flatten.sorted.map(_.simple).mkString("\n")}")
        newBestSolution.foreach: newValue =>
          if (newValue.currentPressure > bestSolutionFound.map(_.currentPressure).getOrElse(0))
            loggerAOCPart1.trace(newValue.simple)
        loggerAOCPart1.trace(s"${newBestSolution.map(_.currentPressure)} and ${newPartialResults.filter(_.isASolution).map(Some(_)).flatten.sorted.lastOption}")
      }

      val nextPResultsUnfiltered = (tail ::: newPartialResults.filterNot(_.isASolution)).sorted.reverse
      val nextPResults = newBestSolution match
        case Some(solution) => nextPResultsUnfiltered.filter(_.maxPotential > solution.currentPressure)
        case None => nextPResultsUnfiltered

      solveMaze(nextPResults, newBestSolution)


@tailrec
def solveMaze2(currentPResults: List[PartialResultPart2], bestSolutionFound: Option[PartialResultPart2], alreadyManaged: List[PartialResultPart2] = Nil)(using graph: ValveGraph): Int =
  currentPResults match
    case Nil => bestSolutionFound match
      case Some(value) => value.currentPressure
      case None => throw Exception("Not Found")
    case head :: tail =>
      loggerAOCPart1.trace(s"IN --> ${head.simple}")
      loggerAOCPart2.whenDebugEnabled {
        if (round < 15)
          //loggerAOCPart2.debug(s"${currentPResults.map(_.simple)}")
          loggerAOCPart2.debug(s"${head.simple}")
          round += 1
        if (head.simple.contains("(DD,JJ)") || head.simple.contains("(JJ,DD)"))
          loggerAOCPart2.debug(s"${head.simple}")
      }

      val newPartialResults = head.next.distinct diff alreadyManaged
      loggerAOCPart1.whenTraceEnabled {
        newPartialResults.foreach: current =>
          loggerAOCPart1.trace(current.simple)
      }

      given Ordering[PartialResultPart2] = Heuristic1.ByPotentialFirst

      val newBestSolution = (bestSolutionFound +: newPartialResults.filter(_.isASolution).map(Some(_))).flatten.sorted.reverse.headOption
      loggerAOCPart2.whenDebugEnabled {
        loggerAOCPart2.trace(s"----------> ${newPartialResults.filter(_.isASolution).map(Some(_)).flatten.sorted.map(_.simple).mkString("\n")}")
        newBestSolution.foreach: newValue =>
          if (newValue.currentPressure > bestSolutionFound.map(_.currentPressure).getOrElse(0))
            loggerAOCPart2.debug(newValue.simple)
        loggerAOCPart2.trace(s"${newBestSolution.map(_.currentPressure)} and ${newPartialResults.filter(_.isASolution).map(Some(_)).flatten.sorted.lastOption}")
      }



      val nextPResultsUnfiltered = (tail ::: newPartialResults.filterNot(_.isASolution)).sorted.reverse
      val nextPResults = newBestSolution match
        case Some(solution) => nextPResultsUnfiltered.filter(_.maxPotential > solution.currentPressure)
        case None => nextPResultsUnfiltered

      solveMaze2(nextPResults, newBestSolution, head +: alreadyManaged)

object Heuristic1:
  object ByPotentialFirst extends Ordering[PartialResultPart2]:
    override def compare(first: PartialResultPart2, second: PartialResultPart2) =
      first.maxPotential.compare(second.maxPotential) match
        case 0 => first.currentPressure.compare(second.currentPressure) match
          case 0 => first.steps.compare(second.steps)
          case value => value
        case value => value

