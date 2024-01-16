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

object Solver:
  def runOn(inputLines: Seq[String]): (String, String) =

    val maze = Maze(
      inputLines.map:
        case s"Valve $name has flow rate=$flow; tunnel leads to valve $valve" => Valve(name, flow.toInt, Seq(valve))
        case s"Valve $name has flow rate=$flow; tunnels lead to valves $valves" => Valve(name, flow.toInt, valves.split(", ").toSeq)
    )

    println(maze)

    given ValveGraph = ValveGraph(maze)

    val start = PartialResult(30, "AA", maze, 0, Nil)

    println(s"${maze.valves(0).name} => ${maze.valves(2).name} : ${Proxy.get(maze.valves(0), maze.valves(2))}")
    println(s"${maze.valves(0).name} => ${maze.valves(6).name} : ${Proxy.get(maze.valves(0), maze.valves(6))}")

    //println(start.maxPotential)

    println(s"Result found = ${solveMaze(List(start), None)}")

    //println(maze.openValve(maze.get("HH")))

    val result1 = s""
    val result2 = s""

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

case class PartialResult(remainingSteps: Int, start: String, maze: Maze, currentPressure: Int, openedValves: List[String])(using graph: ValveGraph) extends Ordered[PartialResult]:
  override def compare(that: PartialResult): Int =
    def mazePressure(pr: PartialResult) = pr.maze.get(pr.start).pressure
    currentPressure.compare(that.currentPressure) match
      case 0 => this.maxPotential.compare(that.maxPotential) match
        case 0 => mazePressure(this).compare(mazePressure(that))
        case value => value
      case value => value
  lazy val simple: String = s"$start <-> $currentPressure <-> $openedValves <-> $remainingSteps <+> $maxPotential"
  lazy val maxPotential2: Int =
    currentPressure + maze.valves.sortBy(_.pressure).reverse.zipWithIndex.map: (valve, index) =>
      valve.pressure * math.max(remainingSteps - index * 2, 0)
    .sum
  lazy val maxPotential: Int =
    currentPressure + distancesFrom(maze.get(start), remainingSteps - 1).map: (valve, distance) =>
      valve.pressure * math.max(remainingSteps - distance - 1, 0)
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
      //println(s"IN --> ${head.simple}")
      val newPartialResults = head.next
      //newPartialResults.foreach: current =>
        //println(current.simple)
      val newBestSolution = (bestSolutionFound +: newPartialResults.filter(_.isASolution).map(Some(_))).flatten.sorted.lastOption
      //println(s"----------> ${newPartialResults.filter(_.isASolution).map(Some(_)).flatten.sorted.map(_.simple).mkString("\n")}")
      newBestSolution.foreach: newValue =>
        if (newValue.currentPressure > bestSolutionFound.map(_.currentPressure).getOrElse(0))
          println(newValue.simple)
      //println(s"${newBestSolution.map(_.currentPressure)} and ${newPartialResults.filter(_.isASolution).map(Some(_)).flatten.sorted.lastOption}")
      val nextPResultsUnfiltered = (tail ::: newPartialResults.filterNot(_.isASolution)).sorted.reverse
      val nextPResults = newBestSolution match
        case Some(solution) => nextPResultsUnfiltered.filter(_.maxPotential > solution.currentPressure)
        case None => nextPResultsUnfiltered

      solveMaze(nextPResults, newBestSolution)
