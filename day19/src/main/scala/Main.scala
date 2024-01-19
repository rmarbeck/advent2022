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

    //blueprints.par.foreach(current => println(s" =>  ${current.id * current.collectingPotential(24)}"))
    val result = blueprints.par.map(current => current.id * current.collectingPotential(24)).sum

    println(s"$result")


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

sealed trait RobotCost

case class OreRobotCost(ore: Int) extends RobotCost
case class ClayRobotCost(ore: Int) extends RobotCost
case class ObsidianRobotCost(ore: Int, clay: Int) extends RobotCost
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
  def generate: Resources = Resources(oreRobots, clayRobots, obsidianRobots, geodeRobots)
  def addOre =
    loggerAOCPart1.trace("adding oreRobot")
    this.copy(oreRobots = oreRobots + 1)

  def addClay =
    loggerAOCPart1.trace("adding clayRobot")
    this.copy(clayRobots = clayRobots + 1)

  def addObsidian =
    loggerAOCPart1.trace("adding obsidianRobot")
    this.copy(obsidianRobots = obsidianRobots + 1)

  def addGeode =
    loggerAOCPart1.trace("adding geodeRobot")
    this.copy(geodeRobots = geodeRobots + 1)

object MachineInventory:
  def start = MachineInventory(1, 0 ,0 ,0)

enum ToDo:
  case AddOreRobot, AddClayRobot, AddObsidianRobot, AddGeodeRobot

export ToDo._

case class BluePrint(id: Int, oreCost: OreRobotCost, clayCost: ClayRobotCost, obsidianCost: ObsidianRobotCost, geodeCost: GeodeRobotCost):
  def decide(resources: Resources, machines: MachineInventory): ToDo =
    val obsidian = geodeCost.obsidian
    val clay = obsidianCost.clay
    val ore = geodeCost.ore + obsidianCost.ore + clayCost.ore + oreCost.ore
    loggerAOCPart1.trace(s"ratios = $obsidian, $clay, $ore")
    loggerAOCPart1.trace(s"${machines.clayRobots/machines.oreRobots}, ${clay/ore}")
    val result = machines.clayRobots / machines.oreRobots <= clay / ore match
      case true => AddClayRobot
      case false => machines.obsidianRobots/machines.oreRobots <= obsidian/ore match
        case true => AddObsidianRobot
        case false => machines.geodeRobots/machines.oreRobots <= 1 match
          case true => AddGeodeRobot
          case false => AddOreRobot
    println(s"$result")
    result
  def canBuild(resources: Resources, robotCost: RobotCost): Boolean = resources.isEnough(robotCost)

  case class PartialResult(remainingMinutes: Int, resources: Resources, machines: MachineInventory)

  @tailrec
  private def computeRec(results: List[PartialResult], best: Int = 0): Int =
    results match
      case Nil => best
      case head :: tail =>
        head.remainingMinutes match
          case 0 =>
            //if (head.resources.geode > best) then println(s"${head.resources.geode}")
            computeRec(tail, math.max(best, head.resources.geode))
          case _ =>
            val (remainingMinutes, resources, machines) = (head.remainingMinutes, head.resources, head.machines)
            val production = machines.generate
            canBuild(resources, geodeCost) match
              case true => computeRec(PartialResult(remainingMinutes - 1, resources + production - geodeCost, machines.addGeode) :: tail, best)
              case false =>
                canBuild(resources, obsidianCost) match
                  case true => computeRec(PartialResult(remainingMinutes - 1, resources + production - obsidianCost, machines.addObsidian) :: tail, best)
                  case false =>
                    var alternatives: List[PartialResult] = List(PartialResult(remainingMinutes - 1, resources + production, machines))
                    if canBuild(resources, clayCost) then alternatives = alternatives :+ PartialResult(remainingMinutes - 1, resources + production - clayCost, machines.addClay)
                    if canBuild(resources, oreCost) then alternatives = alternatives :+ PartialResult(remainingMinutes - 1, resources + production - oreCost, machines.addOre)
                    computeRec(alternatives ::: tail, best)


  def compute(remainingMinutes: Int, resources: Resources, machines: MachineInventory): Int =
    remainingMinutes match
      case 0 => resources.geode
      case _ =>
        val production = machines.generate
        println(s"[${24-remainingMinutes+1}] $machines <-> $resources")
        val (robotCost, toDo) = canBuild(resources, geodeCost) match
          case true => (Some(geodeCost), Some(AddGeodeRobot))
          case false => decide(resources, machines) match
            case AddOreRobot if canBuild(resources, oreCost) => (Some(oreCost), Some(AddOreRobot))
            case AddClayRobot if canBuild(resources, clayCost) => (Some(clayCost), Some(AddClayRobot))
            case AddObsidianRobot if canBuild(resources, obsidianCost) => (Some(obsidianCost), Some(AddObsidianRobot))
            case _ => (None, None)
        val newResources = robotCost.map(resources - _).getOrElse(resources) + production
        val newMachineInventory = toDo.map:
          case AddOreRobot => machines.addOre
          case AddClayRobot => machines.addClay
          case AddObsidianRobot => machines.addObsidian
          case AddGeodeRobot => machines.addGeode
        .getOrElse(machines)
        compute(remainingMinutes - 1, newResources, newMachineInventory)

  def collectingPotential(minutes: Int): Int = computeRec(List(PartialResult(minutes, Resources.empty, MachineInventory.start)))