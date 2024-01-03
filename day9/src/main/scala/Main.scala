import scala.io.Source
import com.typesafe.scalalogging.Logger

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

    val resultPart1 = follow(dirs, List(Position(4,0)), List(Position(4,0)))
    println(follow2(dirs, List(Position(4,0)), List(Position(4,0))).distinct.length)
    loggerAOC.trace(resultPart1.toString)

    val resultPart2 = List()//Rope(9).moveHead(dirs, Nil, Nil)

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

object Position:
  def shareSameRow(positions: List[Position]): Boolean =
    positions.map(_.row).distinct.length == 1
  def shareSameCol(positions: List[Position]): Boolean =
    positions.map(_.col).distinct.length == 1


enum Dir:
  case Up, Right, Down, Left

enum Situation:
  case Superposed, AlignedInSameRow, AlignedInSameCol, MoreThanOneStepFurther, Other

object Dir:
  def fromChar(value: Char): Dir =
    value match
      case 'U' => Up
      case 'R' => Right
      case 'D' => Down
      case 'L' => Left
      case _ => throw Exception("Unknown Dir")

case class Instruction(direction: Dir, steps: Int)

class Rope(val size: Int, val rowInit: Int = 4, val colInit: Int = 0):
  var knots: List[Position] = List.fill(size)(Position(rowInit, colInit))
  def moveHead(directions: List[Dir], previousMoves: List[Dir], tPositions: List[Position]): List[Position] =
    directions match
      case Nil => tPositions
      case headDir :: tailDir =>
        val partialRope = (headDir :: previousMoves).zip(knots.sliding(2,1)).map((dir, currentKnots) => goFrom(dir, currentKnots(0), currentKnots(1))).map(currentPositions => currentPositions._1)
        knots = partialRope ::: knots.drop(partialRope.length)
        //display(5)
        moveHead(tailDir, headDir :: previousMoves, knots(size-1) :: tPositions)

  def display(size: Int): Unit =
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
export Situation._
def follow(directions: List[Dir], hPositions: List[Position], tPositions: List[Position]): List[Position] =
  directions match
    case Nil => tPositions
    case headDir :: tailDir =>
      val (newHPosition, newTPosition) = goFrom(headDir, hPositions.head, tPositions.head)
      //display(newHPosition, newTPosition, 6)
      follow(tailDir, newHPosition :: hPositions, newTPosition :: tPositions)
def follow2(directions: List[Dir], hPositions: List[Position], tPositions: List[Position]): List[Position] =
  directions match
    case Nil => tPositions
    case headDir :: tailDir =>
      val newHPosition = move(headDir, hPositions.head)
      val newTPosition = goFrom2(hPositions.head, newHPosition, tPositions.head)
      //display(newHPosition, newTPosition, 6)
      follow(tailDir, newHPosition :: hPositions, newTPosition :: tPositions)


def goFrom2(oldHPosition: Position, newHPosition: Position, tPosition: Position): Position =
  val newTPosition = getSituation(tPosition, oldHPosition, newHPosition) match
    case AlignedInSameRow | AlignedInSameCol => oldHPosition
    case MoreThanOneStepFurther => oldHPosition
    case _ => tPosition
  newTPosition


def goFrom(dir: Dir, hPosition: Position, tPosition: Position): (Position, Position) =
  val newHPosition = move(dir, hPosition)
  val newTPosition = getSituation(tPosition, hPosition, newHPosition) match
    case AlignedInSameRow | AlignedInSameCol => loggerAOC.trace(s"$dir - same row or col going $dir current is $tPosition"); move(dir, tPosition)
    case MoreThanOneStepFurther => loggerAOC.trace(s"$dir - diag $dir current is $tPosition and $newHPosition");  stick(dir, newHPosition)
    case _ => loggerAOC.trace(s"$dir - doesn't move $dir current is $tPosition"); tPosition
  (newHPosition, newTPosition)


def getSituation(currentTPosition: Position, oldHPosition: Position, newHPosition: Position): Situation =
  (currentTPosition, oldHPosition, newHPosition) match
    case (tpos, oldpos, newpos) if tpos == oldpos || tpos == newpos => Superposed
    case (tpos, oldpos, newpos) if Position.shareSameRow(tpos :: oldpos :: newpos :: Nil) => AlignedInSameRow
    case (tpos, oldpos, newpos) if Position.shareSameCol(tpos :: oldpos :: newpos :: Nil) => AlignedInSameCol
    case (tpos, _, newpos) if tpos.isTwoStepsInDiag(newpos) => MoreThanOneStepFurther
    case _ => Other

def stick(dir: Dir, toPosition: Position): Position =
  dir match
    case Up => toPosition.copy(row = toPosition.row + 1)
    case Right => toPosition.copy(col = toPosition.col - 1)
    case Down => toPosition.copy(row = toPosition.row - 1)
    case Left => toPosition.copy(col = toPosition.col + 1)

def move(dir: Dir, fromPosition: Position): Position =
  dir match
    case Up => fromPosition.copy(row = fromPosition.row - 1)
    case Right => fromPosition.copy(col = fromPosition.col + 1)
    case Down => fromPosition.copy(row = fromPosition.row + 1)
    case Left => fromPosition.copy(col = fromPosition.col - 1)