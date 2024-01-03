import scala.io.Source
import com.typesafe.scalalogging.Logger

import scala.annotation.tailrec

val loggerAOC = Logger("aoc")
val loggerAOCPart1 = Logger("aoc.part1")
val loggerAOCPart2 = Logger("aoc.part2")

@main def hello: Unit =
  loggerAOC.trace("Root trace activated")
  loggerAOC.debug("Root debug activated")
  println("Launching Day9")
  List[() => (String, String)]( () => Solver.solveTest, () => Solver.solve).foreach: f =>
    val (score1, score2) = f.apply()
    println(s"1 : ${score1}")
    println(s"2 : ${score2}")
    println(s"----------------")
  println("Done")

object Solver:
  def runOn(inputLines: Seq[String]): (String, String) =
    val dirs = inputLines.flatMap:
      case s"$dir $steps" => List.fill(steps.toInt)(Dir.fromChar(dir.head))
    .toList

    val resultPart1 = Position(4, 0).follow(dirs)
    loggerAOCPart1.trace(resultPart1.toString)

    val resultPart2 = Rope(10, 15, 15).follow(dirs)
    loggerAOCPart2.trace(resultPart2.toString)

    val result1 = s"${resultPart1.distinct.length}"
    val result2 = s"${resultPart2.distinct.length}"

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

case class Position(row: Int, col: Int):
  def isTwoStepsInDiag(other: Position): Boolean = math.abs((this.row - other.row) * (this.col - other.col)) == 2
  def isAbove(otherPosition: Position): Boolean =
    this.row < otherPosition.row
  def isAtLeft(otherPosition: Position): Boolean =
    this.col < otherPosition.col
  def follow(directions: List[Dir]): List[Position] =
    follow(directions, this, List(this))

  private def follow(directions: List[Dir], hPosition: Position, tPositions: List[Position]): List[Position] =
    directions match
      case Nil => tPositions
      case headDir :: tailDir =>
        val newHPosition = move(headDir, hPosition)
        val newTPosition = goFrom(newHPosition, tPositions.head)
        //display(newHPosition, newTPosition, 6)
        follow(tailDir, newHPosition, newTPosition :: tPositions)

object Position:
  def shareSameRow(positions: List[Position]): Boolean =
    positions.map(_.row).distinct.length == 1
  def shareSameCol(positions: List[Position]): Boolean =
    positions.map(_.col).distinct.length == 1
  def movedDiagonally(oldPosition: Position, newPosition: Position): Boolean =
    oldPosition.row != newPosition.row && oldPosition.col != newPosition.col

enum Dir:
  case Up, Right, Down, Left

object Dir:
  def fromChar(value: Char): Dir =
    value match
      case 'U' => Up
      case 'R' => Right
      case 'D' => Down
      case 'L' => Left
      case _ => throw Exception("Unknown Dir")

class Rope(val knots: List[Position], val rowInit: Int, val colInit: Int):
  def this(size: Int, rowInit: Int = 4, colInit: Int = 0) = this(List.fill(size)(Position(rowInit, colInit)), rowInit, colInit)

  def follow(directions: List[Dir]): List[Position] =
    moveHead(directions, knots, Nil)

  @tailrec
  private final def propagate(initOfNewRope: List[Position], currentRope: List[Position]) : List[Position] =
    initOfNewRope.length match
      case length if length == currentRope.length => initOfNewRope
      case length =>
        val newTailPosition = goFrom(initOfNewRope(length-1), currentRope(length))
        propagate(initOfNewRope :+ newTailPosition, currentRope)

  @tailrec
  final def moveHead(directions: List[Dir], rope: List[Position], tPositions: List[Position]): List[Position] =
    directions match
      case Nil => tPositions
      case headDir :: tailDir =>
        loggerAOCPart2.trace(s"$headDir : ${rope.head}")
        val newHeadOfRope = move(headDir, rope.head)
        loggerAOCPart2.trace(s"$headDir : ${newHeadOfRope}")
        val updatedRope = propagate(newHeadOfRope :: Nil, rope)
        //Rope(updatedRope, 4, 0).display(30)
        moveHead(tailDir, updatedRope, updatedRope.last :: tPositions)

  def display(size: Int): Unit =
    println("*"*size)
    for row <- 0 until size
        col <- 0 until size
    do
      (row, col) match
        case (cRow, cCol) if knots(0).row == cRow && knots(0).col == cCol => print("H")
        case (cRow, cCol) if knots.tail.count(current => current.row == cRow && current.col == cCol) != 0 => print(knots.tail.zipWithIndex.find(current => current._1.row == cRow && current._1.col == cCol).get._2 + 1)
        case _ => print(".")
      if col == size - 1 then println


def display(hPosition: Position, tPosition: Position, size: Int): Unit =
  for row <- 0 until size
      col <- 0 until size
  do
    (row, col) match
      case (cRow, cCol) if hPosition.row == cRow && hPosition.col == cCol => print("H")
      case (cRow, cCol) if tPosition.row == cRow && tPosition.col == cCol => print("T")
      case _ => print(".")
    if col == size - 1 then println

export Dir._

def goFrom(newHPosition: Position, tPosition: Position): Position =
  def touchThemselves: Boolean =
    val rowTouch = newHPosition.row <= tPosition.row + 1 && newHPosition.row >= tPosition.row - 1
    val colTouch = newHPosition.col <= tPosition.col + 1 && newHPosition.col >= tPosition.col - 1
    rowTouch && colTouch

  touchThemselves match
    case true => tPosition
    case false =>
      (Position.shareSameRow(newHPosition :: tPosition :: Nil), Position.shareSameCol(newHPosition :: tPosition :: Nil)) match
        case (true, true) => tPosition
        case (false, true) => tPosition.copy(row = (newHPosition.row + tPosition.row)/2)
        case (true, false) => tPosition.copy(col = (newHPosition.col + tPosition.col)/2)
        case _ => (newHPosition.isAbove(tPosition), newHPosition.isAtLeft(tPosition)) match
          case (true, true) => tPosition.copy(row = tPosition.row - 1, col = tPosition.col - 1)
          case (true, false) => tPosition.copy(row = tPosition.row - 1, col = tPosition.col + 1)
          case (false, true) => tPosition.copy(row = tPosition.row + 1, col = tPosition.col - 1)
          case _ => tPosition.copy(row = tPosition.row + 1, col = tPosition.col + 1)

def move(dir: Dir, fromPosition: Position): Position =
  dir match
    case Up => fromPosition.copy(row = fromPosition.row - 1)
    case Right => fromPosition.copy(col = fromPosition.col + 1)
    case Down => fromPosition.copy(row = fromPosition.row + 1)
    case Left => fromPosition.copy(col = fromPosition.col - 1)