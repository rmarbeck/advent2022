import scala.io.Source
import com.typesafe.scalalogging.Logger

import scala.collection.immutable.HashMap

val loggerAOC = Logger("aoc")
val loggerAOCPart1 = Logger("aoc.part1")
val loggerAOCPart2 = Logger("aoc.part2")

@main def hello: Unit =
  loggerAOC.trace("Root trace activated")
  loggerAOC.debug("Root debug activated")
  println("Launching Day23")
  List[() => (String, String)]( () => Solver.solveTest, () => Solver.solve).foreach: f =>
    val (score1, score2) = f.apply()
    println(s"1 : ${score1}")
    println(s"2 : ${score2}")
    println(s"----------------")
  println("Done")

object Solver:
  def runOn(inputLines: Seq[String]): (String, String) =

    val part1NumberOfRounds = 9

    val elves = inputLines.zipWithIndex.flatMap:
      case (line, row) => line.zipWithIndex.foldLeft(List[Elf]()):
        case (acc, (character, col)) => character match
          case '#' => Elf(Position(row, col)) +: acc
          case _ => acc

    val initialGrove = Grove(elves)
    val strats = strategies()

    val groveEmptyPlaces = groves(initialGrove, strats)

    val result1 = s"${groveEmptyPlaces(part1NumberOfRounds)}"
    val result2 = s"${groveEmptyPlaces.length + 1}"

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

sealed trait Strategy:
  def placesToLookFor(elf: Elf)(using grove: Grove): List[(Int, Int)]
  private final def positionsToLookFor(elf: Elf)(using grove: Grove) = placesToLookFor(elf).map(coords => Position(coords._1, coords._2))
  def canMove(elf: Elf)(using grove: Grove): Boolean = ! grove.isOneOccupied(positionsToLookFor(elf))
  def move(elf: Elf): Position

class NorthFirst extends Strategy:
  override def placesToLookFor(elf: Elf)(using grove: Grove) =
    val Position(eRow, eCol) = elf.position
    (eCol - 1 to eCol + 1).map((eRow-1, _)).toList
  override def move(elf: Elf): Position = elf.moveNorth.position

class SouthFirst extends Strategy:
  override def placesToLookFor(elf: Elf)(using grove: Grove) =
    val Position(eRow, eCol) = elf.position
    (eCol - 1 to eCol + 1).map((eRow + 1, _)).toList
  override def move(elf: Elf): Position = elf.moveSouth.position

class EastFirst extends Strategy:
  override def placesToLookFor(elf: Elf)(using grove: Grove) =
    val Position(eRow, eCol) = elf.position
    (eRow - 1 to eRow + 1).map((_, eCol + 1)).toList
  override def move(elf: Elf): Position = elf.moveEast.position

class WestFirst extends Strategy:
  override def placesToLookFor(elf: Elf)(using grove: Grove) =
    val Position(eRow, eCol) = elf.position
    (eRow - 1 to eRow + 1).map((_, eCol - 1)).toList
  override def move(elf: Elf): Position = elf.moveWest.position

case class Position(row: Int, col: Int)

case class Elf(position: Position):
  def moveNorth: Elf = Elf(position.copy(row = position.row - 1))
  def moveSouth: Elf = Elf(position.copy(row = position.row + 1))
  def moveEast: Elf = Elf(position.copy(col = position.col + 1))
  def moveWest: Elf = Elf(position.copy(col = position.col - 1))
  lazy val around: List[Position] =
    (for row <- position.row - 1 to position.row + 1
         col <- position.col - 1 to position.col + 1
         if (Position(row, col) != this.position)
    yield
      Position(row, col)
    ).toList

  def shouldMove(using grove: Grove): Boolean = grove.isOneOccupied(around)

  def preferedMove(strategies: List[Strategy])(using grove: Grove): Position =
      strategies.find(_.canMove(this)) match
        case Some(strategy) => strategy.move(this)
        case None => this.position

case class Grove(elves: Seq[Elf]):
  def isOneOccupied(positions: List[Position]): Boolean = positions.map(hash(_)).exists(hashed.contains(_))
  def emptyPlaces: Int = (height * width) - elves.length
  private lazy val (minRow, maxRow) =
    val rows = elves.map(_.position.row).sorted
    (rows.head, rows.last)
  private lazy val (minCol, maxCol) =
    val cols = elves.map(_.position.col).sorted
    (cols.head, cols.last)
  val height = maxRow - minRow + 1
  val width = maxCol - minCol + 1

  val hashed: HashMap[Long, Elf] = HashMap(elves.map(elf => hashElf(elf) -> elf): _*)

  def hash(position: Position): Long =
    val (driftedRow, driftedCol) = (position.row - (minRow - 1), position.col - (minCol - 1))
    height > width match
      case true => driftedCol * (height + 2) + driftedRow
      case false => driftedRow * (width + 2) + driftedCol

  def hashElf(elf: Elf): Long = hash(elf.position)


  override def toString: String =
    val singleRow =
      (for row <- elves.map(_.position.row).min to elves.map(_.position.row).max
        col <- elves.map(_.position.col).min to elves.map(_.position.col).max
      yield
        elves.exists(current => current.position == Position(row, col)) match
          case true => '#'
          case false => '.'
      ).mkString

    singleRow.grouped(width).mkString("\n")


def strategies(start: Int = 0): LazyList[List[Strategy]] =
  val raw = List(NorthFirst(), SouthFirst(), WestFirst(), EastFirst())
  val modulo = raw.length
  (raw.drop(start%modulo) ::: raw.take(start%modulo)) #:: strategies(start + 1)

def groves(previousGrove: Grove, strategies: Seq[List[Strategy]]): LazyList[Int] =
  val newGrove =
    val elves = previousGrove.elves
    given Grove = previousGrove
    val currentStrategies = strategies.head
    val (movable, unmovable) = elves.partition(_.shouldMove)
    val (movers, nonMovers) = movable.groupBy(_.preferedMove(currentStrategies)).partition:
      case (position, elves) => elves.length == 1

    loggerAOCPart2.debug(s"${movable.length}, ${movers.size}, ${unmovable.length}")
    loggerAOCPart2.debug(s"${movers.keys}")
    loggerAOCPart2.debug(s"${movers.values}")
    val finalListOfElves = unmovable :++ nonMovers.values.flatten :++ movers.keys.map(Elf(_))

    Grove(finalListOfElves)
  newGrove == previousGrove match
    case true =>  LazyList.empty
    case false => LazyList.cons(newGrove.emptyPlaces, groves(newGrove, strategies.tail))