import scala.io.Source
import com.typesafe.scalalogging.Logger

import scala.annotation.tailrec

val loggerAOC = Logger("aoc")
val loggerAOCPart1 = Logger("aoc.part1")
val loggerAOCPart2 = Logger("aoc.part2")

@main def hello: Unit =
  loggerAOC.trace("Root trace activated")
  loggerAOC.debug("Root debug activated")
  println("Launching Day22")
  List[() => (String, String)]( () => Solver.solveTest, () => Solver.solve).foreach: f =>
    val (score1, score2) = f.apply()
    println(s"1 : ${score1}")
    println(s"2 : ${score2}")
    println(s"----------------")
  println("Done")

object Solver:
  def runOn(inputLines: Seq[String]): (String, String) =

    val (playgroundInput, pathInput) = inputLines.span(!_.isEmpty)

    val playground = Playground(playgroundInput)

    val path = Path(pathInput.last)

    //println(playground)

    def move(status: Status, nextMove: (Int, Turn))(using playground: Playground, strategy: Strategy): Status =
      def next(position: Position, direction: Direction, maxSteps: Int): Position =
        playground.nextFree(position, maxSteps, direction)

      val Status(currentPosition, currentDirection) = status
      val (maxSteps, turn) = nextMove
      val finalDirection = currentDirection.turn(turn)
      val nextPosition = next(currentPosition, currentDirection, maxSteps)
      Status(nextPosition, finalDirection)

    val start = Status(playground.findStart, Right)
    //println(start)

    val end = path.moves.foldLeft(start):
      case (acc, newMove) =>
        val result = move(acc, newMove)(using playground, Part1)
        //println(s"$newMove => $result")
        result

    val end2 = path.moves.foldLeft(start):
      case (acc, newMove) =>
        val result = move(acc, newMove)(using playground, Part2)
        //println(s"$newMove => $result")
        result

    val tCanvas = CubeCanvas(None, None, None, None, None)
    val testDataCanvas = CubeCanvas(None, None, OneHalf, OneFourth, OneHalf)
    val realDataCanvas = CubeCanvas(None, None, OneHalf, OneHalf, None)

    assert(tCanvas.move(One, Up) == (Four, Up), "1")
    assert(tCanvas.move(Two, Right) == (Six, Up), "2")
    assert(tCanvas.move(Two, Left) == (Five, Up), "3")
    assert(tCanvas.move(Three, Right) == (Six, Left), "4")
    assert(tCanvas.move(Three, Left) == (Five, Right), "5")

    assert(testDataCanvas.move(One, Up) == (Four, Down), "10")
    assert(testDataCanvas.move(Two, Right) == (Six, Down), "12")
    assert(testDataCanvas.move(Three, Down) == (Four, Up), "13")
    assert(testDataCanvas.move(Three, Right) == (Six, Right), "14")
    assert(testDataCanvas.move(Three, Left) == (Five, Up), "15")
    assert(testDataCanvas.move(Five, Up) == (One, Right), "16")
    assert(testDataCanvas.move(Five, Down) == (Three, Right), "17")

    assert(realDataCanvas.move(One, Up) == (Four, Down), "20")
    assert(realDataCanvas.move(Two, Right) == (Six, Up), "22")
    assert(realDataCanvas.move(Three, Down) == (Four, Up), "23")
    assert(realDataCanvas.move(Three, Right) == (Six, Left), "24")
    assert(realDataCanvas.move(Two, Left) == (Five, Down), "25")

    //println(s" END is $end")
    /*println(end.score)
    println(path)*/

    val result1 = s"${end.score}"
    val result2 = s"${end2.score}"

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

enum Direction:
  lazy val modulo = Direction.values.length
  case Right, Down, Left, Up
  def +(faceRotation: FaceRotation) = (0 until faceRotation.ordinal).foldLeft(this):
    case (acc, _) => acc.turn(Clockwise)

  def -(faceRotation: FaceRotation) = (0 until faceRotation.ordinal).foldLeft(this):
    case (acc, _) => acc.turn(CounterClockwise)

  def turn(turnDir: Turn) =
    turnDir match
      case Clockwise => Direction.fromOrdinal((this.ordinal + 1) % modulo)
      case CounterClockwise => Direction.fromOrdinal((this.ordinal - 1 + modulo) % modulo)

export Direction._

enum Tile:
  case Empty, Open, SolidWall

  override def toString: String =
    this match
      case Empty => " "
      case Open => "."
      case SolidWall => "#"

enum Turn:
  case Clockwise, CounterClockwise

object Turn:
  def keys = Array('R', 'L')
  def from(char: Char): Turn =
    char match
      case 'R' => Clockwise
      case 'L' => CounterClockwise
      case _ => throw Exception("Not managed")

export Tile._
export Turn._

case class Path(private val input: String):
  private val steps: List[Int] = input.split(Turn.keys).map(_.toInt).toList
  private val turns: List[Turn] = input.filter(Turn.keys.contains(_)).map(Turn.from(_)).toList

  private val tunedForLastMoveSteps = steps :+ 0
  private val tunedForLastMoveTurns = turns ::: List(Clockwise, CounterClockwise)

  lazy val moves = tunedForLastMoveSteps.zip(tunedForLastMoveTurns)

  override def toString: String = s"$moves"

case class Position(row: Int, col: Int):
  def inCommon(otherPosition: Position): Int =
    (row == otherPosition.row, col == otherPosition.col) match
      case (true, false) => row
      case (false, true) => col
      case _ => throw Exception("Not defined")

object Position:
  def goRight(from: Position): Position = from.copy(col = from.col + 1)
  def goLeft(from: Position): Position = from.copy(col = from.col - 1)
  def goDown(from: Position): Position = from.copy(row = from.row + 1)
  def goUp(from: Position): Position = from.copy(row = from.row - 1)

case class Status(position: Position, direction: Direction):
  lazy val score = (position.row + 1) * 1000 + 4 * (position.col + 1) + direction.ordinal

enum Strategy:
  case Part1, Part2

export Strategy._

case class Playground(private val input: Seq[String]):
  val width = input.map(_.length).max
  val height = input.size
  val data = Array.fill(height, width)(Empty)
  input.zipWithIndex.foreach:
    case (line, index) => line.zipWithIndex.foreach:
      case ('.', indexOfChar) => data(index)(indexOfChar) = Open
      case ('#', indexOfChar) => data(index)(indexOfChar) = SolidWall
      case _ => ()

  def guessCanvas: CubeCanvas =
    width match
      case 16 => CubeCanvas(None, None, OneHalf, OneFourth, OneHalf)
      case 200 => CubeCanvas(None, None, OneHalf, OneHalf, None)
      case _ => throw Exception("Not implemented")
  def findLeftCorners: List[(FaceId, Position)] =
    width match
      case 16 => List((One, Position(0, 8)), (Two, Position(4, 8)), (Three, Position(8, 8)), (Four, Position(4, 0)), (Five, Position(4, 4)), (Six, Position(8, 12)))
      case 200 => List((One, Position(0, 50)), (Two, Position(50, 50)), (Three, Position(100, 50)), (Four, Position(150, 0)), (Five, Position(100, 0)), (Six, Position(8, 100)))
      case _ => throw Exception("Not implemented")
  lazy val cube = Cube(guessCanvas, findLeftCorners)

  def isDefined(position: Position) = data.isDefinedAt(position.row) && data(position.row).isDefinedAt(position.col) && data(position.row)(position.col) != Empty
  def isWall(position: Position) = isDefined(position) && data(position.row)(position.col) == SolidWall
  def jumpToNextPart1(undefinedPosition: Position, fromPosition: Position, direction: Direction): (Position, Direction) =
    def findMostRightOnRow(row: Int) = Position(row, data(row).zipWithIndex.filter(current => current._1 != Empty).map(_._2).last)
    def findMostLeftOnRow(row: Int) = Position(row, data(row).zipWithIndex.filter(current => current._1 != Empty).map(_._2).head)
    def findMostDownOnCol(col: Int) = Position( (0 until height).filter(data(_)(col) != Empty).last, col)
    def findMostUpOnCol(col: Int) = Position((0 until height).filter(data(_)(col) != Empty).head, col)
    val newPosition = undefinedPosition.row - fromPosition.row match
      case 0 => undefinedPosition.col - fromPosition.col match
        case diffOnCOl if diffOnCOl < 0 => findMostRightOnRow(fromPosition.row)
        case _ => findMostLeftOnRow(fromPosition.row)
      case diffOnRow if diffOnRow < 0 => findMostDownOnCol(fromPosition.col)
      case _ => findMostUpOnCol(fromPosition.col)
    (newPosition, direction)

  def jumpToNextPart2(undefinedPosition: Position, fromPosition: Position, direction: Direction): (Position, Direction) =
    println(s"jumpToNextPart2 : $undefinedPosition, $fromPosition, $direction")
    cube.move(fromPosition, direction, undefinedPosition)

  def findStart: Position = Position(0, data(0).indexWhere(_ == Open))

  @tailrec
  final def nextFree(fromPosition: Position, maxSteps: Int, direction: Direction)(using strategy: Strategy = Part1): Position =
    println(s"nextFree : $fromPosition, $maxSteps, $direction, $strategy")
    import Position.*

    val walkThrough = direction match
      case Right => goRight
      case Left => goLeft
      case Up => goUp
      case Down => goDown

    maxSteps match
      case 0 => fromPosition
      case _ =>
        val newPotentialPosition = walkThrough.apply(fromPosition)
        isDefined(newPotentialPosition) match
          case true => isWall(newPotentialPosition) match
            case true => fromPosition
            case false => nextFree(newPotentialPosition, maxSteps - 1, direction)
          case false =>
            val (jumpedPosition, newDirection) = strategy match
              case Part1 => jumpToNextPart1(newPotentialPosition, fromPosition, direction)
              case Part2 => jumpToNextPart2(newPotentialPosition, fromPosition, direction)
            //println(s"Jumped from $fromPosition to $jumpedPosition because of $newPotentialPosition")
            isWall(jumpedPosition) match
              case true => fromPosition
              case false => nextFree(jumpedPosition, maxSteps - 1, direction)

  override def toString: String = data.map(_.mkString).mkString("\n")

enum FaceRotation:
  case None, OneFourth, OneHalf, ThreeFourth

export FaceRotation.*

enum FaceId:
  case One, Two, Three, Four, Five, Six
  def move(direction: Direction): (FaceId, Direction) = VirtualTCube.move(this, direction)

export FaceId.*

case class Cube(canvas: CubeCanvas, leftUpCorners: List[(FaceId, Position)]):
  val faceSize =
    val fromWidth = leftUpCorners.map(_._2.col).max
    val fromHeight = leftUpCorners.map(_._2.row).max
    val List(min, max) = List(fromWidth, fromHeight).sorted
    min / 2 == max / 3 match
      case true => min / 2
      case false => throw Exception(s"Not supported canvas ${fromWidth} != ${fromHeight}")
  def face(position: Position): (FaceId, Position) =
    val face = position match
      case Position(row, col) =>
        println(s"${position} $faceSize -> $leftUpCorners")
        leftUpCorners.find:
          case (_, Position(rowStart, colStart)) =>
            val result = row >= rowStart && row < rowStart + faceSize && col >= colStart && col < colStart + faceSize
            println(s"$result")
            result
        .map(_._1).get
    val relativePosition =
      leftUpCorners.filter(_._1 == face).map:
        case (_, Position(rowStart, colStart)) => Position(position.row - rowStart, position.col - colStart)
      .head
    (face, relativePosition)

  def locate(faceFrom: FaceId, faceTo: FaceId, direction: Direction, fromPosition: Position, undefinedPosition: Position, relativePosition: Position): Position =
    def guessRow: Int = relativePosition.row
    def guessCol: Int = relativePosition.col
    val leftUpCorner = leftUpCorners.filter(_._1 == faceTo).map(_._2).head
    direction match
      case Up => Position(leftUpCorner.row + faceSize - 1, leftUpCorner.col + guessCol)
      case Down => Position(leftUpCorner.row, leftUpCorner.col + guessCol)
      case Left => Position(leftUpCorner.row + guessRow, leftUpCorner.col)
      case Right => Position(leftUpCorner.row + guessRow, leftUpCorner.col + faceSize - 1)
  def move(position: Position, direction: Direction, undefinedPosition: Position): (Position, Direction) =
    val (faceFrom, relativePosition) = face(position)
    val (faceTo, newDirection) = canvas.move(faceFrom, direction)
    (locate(faceFrom, faceTo, newDirection, position, undefinedPosition, relativePosition), newDirection)

class CubeCanvas(val rTwo: FaceRotation, val rThree: FaceRotation, val rFour: FaceRotation, val rFive: FaceRotation, val rSix: FaceRotation):
  def move(faceFrom: FaceId, direction: Direction): (FaceId, Direction) =
    val (faceTo, temporaryDirection) = faceFrom.move(transformOut(faceFrom, direction))
    (faceTo, transformIn(faceTo, temporaryDirection))
  def rotationFactor(faceId: FaceId) = faceId match
    case One => None
    case Two => rTwo
    case Three => rThree
    case Four => rFour
    case Five => rFive
    case Six => rSix
  def transformOut(faceFrom: FaceId, direction: Direction): Direction = direction + rotationFactor(faceFrom)

  def transformIn(faceTo: FaceId, direction: Direction): Direction = direction - rotationFactor(faceTo)

object VirtualTCube:
  def move(faceFrom: FaceId, direction: Direction): (FaceId, Direction) =
    val dirs = List(Up, Right, Down, Left)
    def direct(faceU: FaceId, faceR: FaceId, faceD: FaceId, faceL: FaceId) = dirs.zip(List(faceU, faceR, faceD, faceL)).find(_._1 == direction).map(found => (found._2, false)).get
    def sameDir(faceId: FaceId) = (faceId, direction)
    def altDir(faceId: FaceId) = faceFrom match
      case Two => (faceId, Up)
      case Three => faceId match
        case Six => (faceId, Left)
        case Five => (faceId, Right)
        case _ => throw Exception("Unsupported")
      case Four => (faceId, Down)
      case Five => (faceId, Right)
      case _ => (faceId, Left)
    val (next, mustTurn) = faceFrom match
      case One => direct(Four, Five, Two, Six)
      case Two => direction match
        case Up =>      (One,   false)
        case Right =>   (Six,   true)
        case Down =>    (Three, false)
        case Left =>    (Five,  true)
      case Three => direction match
        case Up =>      (Two,   false)
        case Right =>   (Six,   true) // => Left
        case Down =>    (Four,  false)
        case Left =>    (Five,  true) // => Right
      case Four => direction match
        case Up =>      (Three, false)
        case Right =>   (Six,   true)
        case Down =>    (One,   false)
        case Left =>    (Five,  true)
      case Five => direction match
        case Up =>      (Four,  true)
        case Right =>   (One,   false)
        case Down =>    (Two,   true)
        case Left =>    (Three, true)
      case Six => direction match
        case Up =>      (Four,  true)
        case Right =>   (One,   false)
        case Down =>    (Two,   true)
        case Left =>    (Three, true)
    mustTurn match
      case false => sameDir(next)
      case true =>  altDir(next)
