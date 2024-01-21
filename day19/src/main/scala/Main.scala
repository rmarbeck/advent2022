import scala.io.Source
import com.typesafe.scalalogging.Logger

import scala.annotation.tailrec
import scala.collection.parallel.CollectionConverters.*

val loggerAOC = Logger("aoc")
val loggerAOCPart1 = Logger("aoc.part1")
val loggerAOCPart2 = Logger("aoc.part2")

@main def hello: Unit =
  loggerAOC.trace("Root trace activated")
  loggerAOC.debug("Root debug activated")
  println("Launching Day19")
  List[() => (String, String)]( () => Solver.solveTest, () => Solver.solve).foreach: f =>
    val (score1, score2) = f.apply()
    println(s"1 : ${score1}")
    println(s"2 : ${score2}")
    println(s"----------------")
  println("Done")

object Solver:
  def runOn(inputLines: Seq[String]): (String, String) =
    val blueprints = inputLines.map:
      case s"Blueprint $id: Each ore robot costs $oreRobot ore. Each clay robot costs $clayRobot ore. Each obsidian robot costs $obsidianRobotOre ore and $obsidianRobotClay clay. Each geode robot costs $geodeRobotOre ore and $geodeRobotObsidian obsidian." => List(id, oreRobot, clayRobot, obsidianRobotOre, obsidianRobotClay, geodeRobotOre, geodeRobotObsidian).map(_.toInt) match
        case List(id, oreRobot, clayRobot, obsidianRobotOre, obsidianRobotClay, geodeRobotOre, geodeRobotObsidian) => BluePrint(id, OreRobotCost(oreRobot), ClayRobotCost(clayRobot), ObsidianRobotCost(obsidianRobotOre, obsidianRobotClay), GeodeRobotCost(geodeRobotOre, geodeRobotObsidian))
        case _ => throw Exception("Not supported")

    val resultPart1 = blueprints.par.map(current => current.id * current.collectingPotential(24)).sum

    val resultPart2 = blueprints.take(3).par.map(current => current.collectingPotential(32)).product

    val result1 = s"$resultPart1"
    val result2 = s"$resultPart2"

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

sealed trait RobotCost

case class OreRobotCost(ore: Int) extends RobotCost
case class ClayRobotCost(ore: Int) extends RobotCost
case class ObsidianRobotCost(ore: Int, clay: Int) extends RobotCost:
  def from(resources: Resources): Int = math.min(resources.ore/ore, resources.clay/clay)
case class GeodeRobotCost(ore: Int, obsidian: Int) extends RobotCost

case class Resources(ore: Int, clay: Int, obsidian: Int, geode: Int):
  def isEnough(robotCost: RobotCost) =
    val result = robotCost match
      case OreRobotCost(cost) => ore >= cost
      case ClayRobotCost(cost) => ore >= cost
      case ObsidianRobotCost(oreCost, clayCost) => ore >= oreCost && clay >= clayCost
      case GeodeRobotCost(oreCost, obsidianCost) => ore >= oreCost && obsidian >= obsidianCost
    result
  def -(robotCost: RobotCost) =
    robotCost match
      case OreRobotCost(oreToSubtract) => this.copy(ore = ore - oreToSubtract)
      case ClayRobotCost(oreToSubtract) => this.copy(ore = ore - oreToSubtract)
      case ObsidianRobotCost(oreToSubtract, clayToSubtract) => this.copy(ore = ore - oreToSubtract, clay = clay - clayToSubtract)
      case GeodeRobotCost(oreToSubtract, obsidianToSubtract) => this.copy(ore = ore - oreToSubtract, obsidian = obsidian - obsidianToSubtract)

  def +(other: Resources) = Resources(ore + other.ore, clay + other.clay, obsidian + other.obsidian, geode + other.geode)

object Resources:
  def empty = Resources(0, 0 ,0 ,0)

case class MachineInventory(oreRobots: Int, clayRobots: Int, obsidianRobots: Int, geodeRobots: Int):
  def numberOfMachines = oreRobots + clayRobots + obsidianRobots + geodeRobots
  def generate: Resources = iterate(1)
  def iterate(nTimes: Int): Resources = Resources(oreRobots*nTimes, clayRobots*nTimes, obsidianRobots*nTimes, geodeRobots*nTimes)
  def addOre = this.copy(oreRobots = oreRobots + 1)
  def addClay = this.copy(clayRobots = clayRobots + 1)
  def addObsidian = this.copy(obsidianRobots = obsidianRobots + 1)
  def addGeode = this.copy(geodeRobots = geodeRobots + 1)

object MachineInventory:
  def start = MachineInventory(1, 0 ,0 ,0)

enum ToDo:
  case AddOreRobot, AddClayRobot, AddObsidianRobot, AddGeodeRobot

export ToDo._

case class BluePrint(id: Int, oreCost: OreRobotCost, clayCost: ClayRobotCost, obsidianCost: ObsidianRobotCost, geodeCost: GeodeRobotCost):
  lazy val obsidianNeed = geodeCost.obsidian
  lazy val clayNeed = obsidianCost.clay
  lazy val oreNeed = geodeCost.ore + obsidianCost.ore + clayCost.ore + oreCost.ore

  private def canBuild(resources: Resources, robotCost: RobotCost): Boolean = resources.isEnough(robotCost)

  private case class PartialResult(remainingMinutes: Int, resources: Resources, machines: MachineInventory)

  @tailrec
  private def computeRec(results: List[PartialResult], best: Int = 0): Int =
    results match
      case Nil => best
      case head :: tail =>
        head.remainingMinutes match
          case 0 => computeRec(tail, math.max(best, head.resources.geode))
          case _ =>
            val (remainingMinutes, resources, machines, production) = (head.remainingMinutes, head.resources, head.machines, head.machines.generate)
            canBuild(resources, geodeCost) match
              case true => computeRec(PartialResult(remainingMinutes - 1, resources + production - geodeCost, machines.addGeode) :: tail, best)
              case false =>
                canBuild(resources, obsidianCost) match
                  case true => computeRec(PartialResult(remainingMinutes - 1, resources + production - obsidianCost, machines.addObsidian) :: tail, best)
                  case false =>
                    @tailrec
                    def timeToObsidian(remainingMinutes: Int, resources: Resources, machines: MachineInventory, score: Int = 0): Int =
                      remainingMinutes match
                        case 0 => Int.MaxValue
                        case _ => canBuild(resources, obsidianCost) match
                          case true => score
                          case false => timeToObsidian(remainingMinutes - 1, resources + machines.generate, machines, score + 1)

                    lazy val timeToObsidianWithNewClay = timeToObsidian(remainingMinutes - 1, resources + production - clayCost, machines.addClay)
                    lazy val timeToObsidianWithNewOre = timeToObsidian(remainingMinutes - 1, resources + production - oreCost, machines.addOre)
                    lazy val timeToObsidianWithNothing = timeToObsidian(remainingMinutes - 1, resources + production, machines)

                    def needMoreClayRobots: Boolean = timeToObsidianWithNewClay <= timeToObsidianWithNewOre
                    def needMoreOreRobots: Boolean = timeToObsidianWithNewOre <= timeToObsidianWithNothing
                    def shouldBuildClay: Boolean = canBuild(resources, clayCost) && needMoreClayRobots
                    def shouldBuildOre: Boolean = (oreCost.ore < remainingMinutes) && canBuild(resources, oreCost) && needMoreOreRobots

                    def shouldBuildNothing: Boolean = !shouldBuildOre

                    val noBuild = PartialResult(remainingMinutes - 1, resources + production, machines)
                    val buildClay = PartialResult(remainingMinutes - 1, resources + production - clayCost, machines.addClay)
                    val buildOre = PartialResult(remainingMinutes - 1, resources + production - oreCost, machines.addOre)

                    val tests = List(shouldBuildClay, shouldBuildOre, shouldBuildNothing)
                    val alternatives: List[PartialResult] = List(buildClay, buildOre, noBuild).zip(tests).filter(_._2).map(_._1)
                    computeRec(alternatives ::: tail, best)

  def collectingPotential(minutes: Int): Int = computeRec(List(PartialResult(minutes, Resources.empty, MachineInventory.start)))