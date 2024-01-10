import scala.io.Source
import com.typesafe.scalalogging.Logger

import scala.annotation.tailrec
import scala.collection.immutable.ArraySeq

val loggerAOC = Logger("aoc")
val loggerAOCPart1 = Logger("aoc.part1")
val loggerAOCPart2 = Logger("aoc.part2")

@main def hello: Unit =
  loggerAOC.trace("Root trace activated")
  loggerAOC.debug("Root debug activated")
  println("Launching Day14")
  List[() => (String, String)]( () => Solver.solveTest, () => Solver.solve).foreach: f =>
    val (score1, score2) = f.apply()
    println(s"1 : ${score1}")
    println(s"2 : ${score2}")
    println(s"----------------")
  println("Done")

object Solver:
  def runOn(inputLines: Seq[String]): (String, String) =

    val rocklines = inputLines.map:
      _.split(" -> ").map:
        case s"$col,$row" => PartOfRockLine(col.toInt, row.toInt)
    .map(currentArray => RockLine(currentArray.toIndexedSeq))
    val cavePart1 = Cave(rocklines)(false)
    val cavePart2 = Cave(rocklines)(true)

    cavePart1.acceptSandParticle(SandParticle.fromOrigin)
    cavePart2.acceptSandParticle(SandParticle.fromOrigin)

    val result1 = s"${cavePart1.howManySandParticles}"
    val result2 = s"${cavePart2.howManySandParticles}"

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

case class PartOfRockLine(col: Int, row: Int)

private case class SandParticle(col: Int, row: Int):
  private def nextPositionsPossible(using cave: Cave): List[Boolean] = List(cave.isPositionFree(col, row + 1), cave.isPositionFree(col - 1, row + 1), cave.isPositionFree(col + 1, row + 1))
  def canStillMoveInCave(using cave: Cave): Boolean = nextPositionsPossible.contains(true)

  def moveToNextPosition(using cave: Cave): SandParticle =
    nextPositionsPossible match
      case List(true, _, _) => this.copy(row = row + 1)
      case List(false, true , _) => SandParticle(col - 1, row + 1)
      case _ => SandParticle(col + 1, row + 1)

  def hasFallenBelowLimit(using cave: Cave): Boolean = row >= cave.lowLimit
  def isAlreadyASandParticle(using cave: Cave): Boolean = cave.isOccupiedBySand(col, row)

object SandParticle:
  def fromOrigin = SandParticle(500, 0)

case class RockLine(lines: Seq[PartOfRockLine]):
  lazy val maxCol = lines.map(_.col).max
  lazy val maxRow = lines.map(_.row).max

class Cave(rockLines: Seq[RockLine])(withFloor: Boolean):
  import scala.collection.mutable.ArrayBuffer
  private lazy val maxCol = rockLines.map(_.maxCol).max
  private lazy val maxRow =
    withFloor match
      case true => rockLines.map(_.maxRow).max + 2
      case false => rockLines.map(_.maxRow).max

  private val walls = Array.fill(maxRow + 1)(ArrayBuffer.fill(maxCol + 1)(0))

  for rockLine <- rockLines do
    for Seq(PartOfRockLine(colStart, rowStart), PartOfRockLine(colEnd, rowEnd)) <- rockLine.lines.sliding(2, 1) do
      def lowToHigh(first: Int, second: Int): Range =
        first > second match
          case true => second to first
          case false => first to second
      if (colEnd != colStart)
        lowToHigh(colStart, colEnd).foreach:
          walls(rowStart)(_) = 1
      else
        lowToHigh(rowStart, rowEnd).foreach:
          walls(_)(colStart) = 1

  def lowLimit = maxRow

  def isPositionFree(col: Int, row: Int): Boolean =
    withFloor match
      case true if row == maxRow => false
      case _ =>
        if !walls(row).isDefinedAt(col) then
          walls(row).append(0)
        walls(row)(col) == 0

  def isOccupiedBySand(col: Int, row: Int): Boolean = walls(row)(col) == 2

  def howManySandParticles: Int = walls.flatten.count(_ == 2)

  @tailrec
  final def acceptSandParticle(sandParticle: SandParticle): Unit =
    given Cave = this
    sandParticle.hasFallenBelowLimit match
      case true => ()
      case _ =>
        sandParticle.canStillMoveInCave match
          case true => acceptSandParticle(sandParticle.moveToNextPosition)
          case false if sandParticle.isAlreadyASandParticle => ()
          case _ =>
            walls(sandParticle.row)(sandParticle.col) = 2
            acceptSandParticle(SandParticle.fromOrigin)

  private def asString(data: Array[ArrayBuffer[Char]]) =
    data.map(_.mkString(" ")).mkString("\n")

  private def toDisplay(intValue: Int) : Char =
    intValue match
      case 1 => '#'
      case 2 => 'o'
      case _ => '.'

  override def toString =
    asString(walls.map(_.drop(450).map(toDisplay).map(_.toString.head)))
