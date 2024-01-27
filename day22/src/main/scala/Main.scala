import scala.io.Source
import com.typesafe.scalalogging.Logger

import scala.annotation.{tailrec, targetName}

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
  private def runOn(inputLines: Seq[String]): (String, String) =

    val (playgroundInput, pathInput) = inputLines.span(!_.isEmpty)

    val playground = Playground(playgroundInput)

    val path = Path(pathInput.last)

    def move(status: Status, nextMove: (Int, Turn))(using playground: Playground, strategy: Strategy): Status =
      def next(position: Position, direction: Direction, maxSteps: Int): (Position, Direction) =
        playground.nextFree(position, maxSteps, direction)

      val Status(currentPosition, currentDirection) = status
      val (maxSteps, turn) = nextMove
      val (nextPosition, lastDirection) = next(currentPosition, currentDirection, maxSteps)
      val finalDirection = lastDirection.turn(turn)
      Status(nextPosition, finalDirection)

    val start = Status(playground.findStart, Right)

    val end = path.moves.foldLeft(start):
      case (acc, newMove) => move(acc, newMove)(using playground, Part1)

    val end2 = path.moves.foldLeft(start):
      case (acc, newMove) => move(acc, newMove)(using playground, Part2)

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
  private lazy val modulo = Direction.values.length
  case Right, Down, Left, Up
  @targetName("plus")
  def +(faceRotation: FaceRotation): Direction = (0 until faceRotation.ordinal).foldLeft(this):
    case (acc, _) => acc.turn(Clockwise)

  @targetName("minus")
  def -(faceRotation: FaceRotation): Direction = (0 until faceRotation.ordinal).foldLeft(this):
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
  private val turns: List[Turn] = input.filter(Turn.keys.contains).map(Turn.from).toList

  private val tunedForLastMoveSteps = steps :+ 0
  private val tunedForLastMoveTurns = turns ::: List(Clockwise, CounterClockwise)

  lazy val moves = tunedForLastMoveSteps.zip(tunedForLastMoveTurns)

  override def toString: String = s"$moves"

case class Position(row: Int, col: Int):
  @targetName("minus")
  def -(otherPosition: Position): (Int, Int) = (row - otherPosition.row, col - otherPosition.col)

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
  private val width = input.map(_.length).max
  private val height = input.size
  private val data = Array.fill(height, width)(Empty)
  input.zipWithIndex.foreach:
    case (line, index) => line.zipWithIndex.foreach:
      case ('.', indexOfChar) => data(index)(indexOfChar) = Open
      case ('#', indexOfChar) => data(index)(indexOfChar) = SolidWall
      case _ => ()

  private def guessCanvas: CubeCanvas =
    height match
      case 12 => CubeCanvas(NoneRotation, NoneRotation, OneHalf, OneFourth, OneHalf)
      case 200 => CubeCanvas(NoneRotation, NoneRotation, ThreeFourth, OneHalf, NoneRotation)
      case _ => throw Exception(s"guessCanvas: Not implemented : $height")
  private def findLeftCorners: List[(FaceId, Position)] =
    height match
      case 12 => List((One, Position(0, 8)), (Two, Position(4, 8)), (Three, Position(8, 8)), (Four, Position(4, 0)), (Five, Position(4, 4)), (Six, Position(8, 12)))
      case 200 => List((One, Position(0, 50)), (Two, Position(50, 50)), (Three, Position(100, 50)), (Four, Position(150, 0)), (Five, Position(100, 0)), (Six, Position(0, 100)))
      case _ => throw Exception("findLeftCorners: Not implemented")
  lazy val cube = Cube(guessCanvas, findLeftCorners)

  private def isDefined(position: Position) = data.isDefinedAt(position.row) && data(position.row).isDefinedAt(position.col) && data(position.row)(position.col) != Empty
  private def isWall(position: Position) = isDefined(position) && data(position.row)(position.col) == SolidWall
  private def jumpToNextPart1(undefinedPosition: Position, fromPosition: Position, direction: Direction): (Position, Direction) =
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

  private def jumpToNextPart2(undefinedPosition: Position, fromPosition: Position, direction: Direction): (Position, Direction) =
    cube.move(fromPosition, direction, undefinedPosition)

  def findStart: Position = Position(0, data(0).indexWhere(_ == Open))

  @tailrec
  final def nextFree(fromPosition: Position, maxSteps: Int, direction: Direction)(using strategy: Strategy = Part1): (Position, Direction) =
    import Position.*

    val walkThrough = direction match
      case Right => goRight
      case Left => goLeft
      case Up => goUp
      case Down => goDown

    maxSteps match
      case 0 => (fromPosition, direction)
      case _ =>
        val newPotentialPosition = walkThrough.apply(fromPosition)
        isDefined(newPotentialPosition) match
          case true => isWall(newPotentialPosition) match
            case true => (fromPosition, direction)
            case false => nextFree(newPotentialPosition, maxSteps - 1, direction)
          case false =>
            val (jumpedPosition, newDirection) = strategy match
              case Part1 => jumpToNextPart1(newPotentialPosition, fromPosition, direction)
              case Part2 => jumpToNextPart2(newPotentialPosition, fromPosition, direction)
            isWall(jumpedPosition) match
              case true => (fromPosition, direction)
              case false => nextFree(jumpedPosition, maxSteps - 1, newDirection)

  override def toString: String = data.map(_.mkString).mkString("\n")

enum FaceRotation:
  case NoneRotation, OneFourth, OneHalf, ThreeFourth

export FaceRotation.*

enum FaceId:
  case One, Two, Three, Four, Five, Six
  def move(direction: Direction): (FaceId, Direction) = VirtualTCube.move(this, direction)

export FaceId.*

case class Cube(canvas: CubeCanvas, leftUpCorners: List[(FaceId, Position)]):
  private val faceSize =
    val fromWidth = leftUpCorners.map(_._2.col).max
    val fromHeight = leftUpCorners.map(_._2.row).max
    val List(min, max) = List(fromWidth, fromHeight).sorted
    min / 2 == max / 3 match
      case true => min / 2
      case false => throw Exception(s"Not supported canvas ${fromWidth} != ${fromHeight}")
  private def face(position: Position): (FaceId, Position) =
    val face = position match
      case Position(row, col) =>
        leftUpCorners.find:
          case (_, Position(rowStart, colStart)) => row >= rowStart && row < rowStart + faceSize && col >= colStart && col < colStart + faceSize
        .map(_._1).get
    val relativePosition =
      leftUpCorners.filter(_._1 == face).map:
        case (_, Position(rowStart, colStart)) => Position(position.row - rowStart, position.col - colStart)
      .head
    (face, relativePosition)

  private def locate(faceTo: FaceId, newDirection: Direction, originalDirection: Direction, relativePosition: Position, useRowInformation: Boolean): Position =
    def getRelativePositionMeaningFull =
      useRowInformation match
        case true => relativePosition.row
        case false => relativePosition.col
    def getRelativePositionDrifted =
      useRowInformation match
        case true => faceSize - 1 - relativePosition.row
        case false => faceSize - 1 - relativePosition.col
    def guessRow: Int =
      (originalDirection, newDirection) match
        case (Up, Right) | (Down, Left) | (Left, Left) | (Right, Right) => getRelativePositionMeaningFull
        case _ => getRelativePositionDrifted
    def guessCol: Int =
      (originalDirection, newDirection) match
        case (Left, Down) | (Right, Up) | (Up, Up) | (Down, Down) => getRelativePositionMeaningFull
        case _ => getRelativePositionDrifted
    val leftUpCorner = leftUpCorners.filter(_._1 == faceTo).map(_._2).head
    newDirection match
      case Up => Position(leftUpCorner.row + faceSize - 1, leftUpCorner.col + guessCol)
      case Down => Position(leftUpCorner.row, leftUpCorner.col + guessCol)
      case Left => Position(leftUpCorner.row + guessRow, leftUpCorner.col + faceSize - 1)
      case Right => Position(leftUpCorner.row + guessRow, leftUpCorner.col)
  def move(position: Position, direction: Direction, undefinedPosition: Position): (Position, Direction) =
    val (faceFrom, relativePosition) = face(position)
    val (faceTo, newDirection) = canvas.move(faceFrom, direction)
    val useRowInformation =
      undefinedPosition - position match
        case (0, _) => true
        case (_, 0) => false
        case _ => throw Exception("Unexpected result")
    val newLocation = locate(faceTo, newDirection, direction, relativePosition, useRowInformation)

    (newLocation, newDirection)

class CubeCanvas(val rTwo: FaceRotation, val rThree: FaceRotation, val rFour: FaceRotation, val rFive: FaceRotation, val rSix: FaceRotation):
  def move(faceFrom: FaceId, direction: Direction): (FaceId, Direction) =
    val (faceTo, temporaryDirection) = faceFrom.move(transformOut(faceFrom, direction))
    (faceTo, transformIn(faceTo, temporaryDirection))
  private def rotationFactor(faceId: FaceId) = faceId match
    case One => NoneRotation
    case Two => rTwo
    case Three => rThree
    case Four => rFour
    case Five => rFive
    case Six => rSix
  private def transformOut(faceFrom: FaceId, direction: Direction): Direction = direction + rotationFactor(faceFrom)

  private def transformIn(faceTo: FaceId, direction: Direction): Direction = direction - rotationFactor(faceTo)

object VirtualTCube:
  def move(faceFrom: FaceId, direction: Direction): (FaceId, Direction) =
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
      case One => direction match
        case Up =>      (Four,   false)
        case Right =>   (Six,   false)
        case Down =>    (Two, false)
        case Left =>    (Five,  false)
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
        case Right =>   (Three, true)
        case Down =>    (Two,   true)
        case Left =>    (One,   false)
    mustTurn match
      case false => sameDir(next)
      case true =>  altDir(next)