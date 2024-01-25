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

    Holder.empty
    //println(playground)

    def move(status: Status, nextMove: (Int, Turn))(using playground: Playground, strategy: Strategy): Status =
      def next(position: Position, direction: Direction, maxSteps: Int): (Position, Direction) =
        playground.nextFree(position, maxSteps, direction)

      val Status(currentPosition, currentDirection) = status
      val (maxSteps, turn) = nextMove
      val (nextPosition, lastDirection) = next(currentPosition, currentDirection, maxSteps)
      val finalDirection = lastDirection.turn(turn)
      Status(nextPosition, finalDirection)

    val start = Status(playground.findStart, Right)
    //println(start)

    val end = path.moves.foldLeft(start):
      case (acc, newMove) =>
        val result = move(acc, newMove)(using playground, Part1)
        //println(s"$newMove => $result")
        result

    Holder.empty
    val end2 = path.moves.foldLeft(start):
      case (acc, newMove) =>
        val result = move(acc, newMove)(using playground, Part2)
        //println(s"$newMove => $result")
        result

    val steps = Holder.path.length
    val reversePath = Holder.path.reverse
    (0 until steps).foreach:
      case index =>
        print("\u001b[2J")
        playground.printPath(reversePath.take(index))
        Thread.sleep(500)



    val tCanvas = CubeCanvas(NoneRotation, NoneRotation, NoneRotation, NoneRotation, NoneRotation)
    val testDataCanvas = CubeCanvas(NoneRotation, NoneRotation, OneHalf, OneFourth, OneHalf)
    val realDataCanvas = CubeCanvas(NoneRotation, NoneRotation, ThreeFourth, OneHalf, NoneRotation)

    assert(DriftOneFourth + ThreeFourth == Aligned)
    assert(DriftOneFourth - ThreeFourth == DriftOneHalf)
    assert(DriftOneHalf + ThreeFourth == DriftOneFourth)

    assert(tCanvas.move(One, Up) == (Four, Up, Aligned), "1")
    assert(tCanvas.move(Two, Right) == (Six, Up, DriftOneFourth), "2")
    assert(tCanvas.move(Two, Left) == (Five, Up, DriftThreeFourth), "3")
    assert(tCanvas.move(Three, Right) == (Six, Left, DriftOneHalf), "4")
    assert(tCanvas.move(Three, Left) == (Five, Right, DriftOneHalf), "5")

    assert(testDataCanvas.move(One, Up) == (Four, Down, DriftOneHalf), "10")
    assert(testDataCanvas.move(Two, Right) == (Six, Down, DriftThreeFourth), "12")
    assert(testDataCanvas.move(Three, Down) == (Four, Up, DriftOneHalf), "13")
    assert(testDataCanvas.move(Three, Right) == (Six, Right, Aligned), "14")
    assert(testDataCanvas.move(Three, Left) == (Five, Up, DriftThreeFourth), s"15 ${testDataCanvas.move(Three, Left)}")
    assert(testDataCanvas.move(Five, Up) == (One, Right, DriftThreeFourth), "16")
    assert(testDataCanvas.move(Five, Down) == (Three, Right, DriftOneFourth), "17")

    assert(realDataCanvas.move(One, Up) == (Four, Right, DriftThreeFourth), s"20 ${realDataCanvas.move(One, Up)}")
    assert(realDataCanvas.move(Four, Left) == (One, Down, DriftThreeFourth), s"20b ${realDataCanvas.move(One, Up)}")
    assert(realDataCanvas.move(Six, Up) == (Four, Up, Aligned), s"21 ${realDataCanvas.move(Six, Up)}")
    assert(realDataCanvas.move(Two, Right) == (Six, Up, DriftOneFourth), "22")
    assert(realDataCanvas.move(Three, Down) == (Four, Left, DriftThreeFourth), "23")
    assert(realDataCanvas.move(Three, Right) == (Six, Left, DriftOneHalf), "24")
    assert(realDataCanvas.move(Two, Left) == (Five, Down, DriftOneFourth), "25")

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
  def -(otherPosition: Position): (Int, Int) = (row - otherPosition.row, col - otherPosition.col)
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
    height match
      case 12 => CubeCanvas(NoneRotation, NoneRotation, OneHalf, OneFourth, OneHalf)
      case 200 => CubeCanvas(NoneRotation, NoneRotation, ThreeFourth, OneHalf, NoneRotation)
      case _ => throw Exception(s"guessCanvas: Not implemented : $height")
  def findLeftCorners: List[(FaceId, Position)] =
    height match
      case 12 => List((One, Position(0, 8)), (Two, Position(4, 8)), (Three, Position(8, 8)), (Four, Position(4, 0)), (Five, Position(4, 4)), (Six, Position(8, 12)))
      case 200 => List((One, Position(0, 50)), (Two, Position(50, 50)), (Three, Position(100, 50)), (Four, Position(150, 0)), (Five, Position(100, 0)), (Six, Position(0, 100)))
      case _ => throw Exception("findLeftCorners: Not implemented")
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
    loggerAOCPart2.debug(s"jumpToNextPart2 : $undefinedPosition, $fromPosition, $direction")
    cube.move(fromPosition, direction, undefinedPosition)

  def findStart: Position = Position(0, data(0).indexWhere(_ == Open))

  @tailrec
  final def nextFree(fromPosition: Position, maxSteps: Int, direction: Direction)(using strategy: Strategy = Part1): (Position, Direction) =
    Holder.add(Status(fromPosition, direction))
    loggerAOCPart2.trace(s"nextFree : $fromPosition, $maxSteps, $direction, $strategy")
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
            //println(s"Jumped from $fromPosition to $jumpedPosition because of $newPotentialPosition")
            isWall(jumpedPosition) match
              case true =>
                loggerAOCPart2.trace(s"Hitting wall on $jumpedPosition")
                (fromPosition, direction)
              case false => nextFree(jumpedPosition, maxSteps - 1, newDirection)

  override def toString: String = data.map(_.mkString).mkString("\n")

  def printPath(path: List[Status]): Unit =
    val newData = Array.tabulate(height, width):
      case (row, col) =>
        path.findLast(current => current.position == Position(row, col)) match
          case Some(Status(_, direction)) => direction match
            case Up => "^"
            case Down => "v"
            case Right => ">"
            case Left => "<"
          case None => data(row)(col)
    println(newData.map(_.mkString).mkString("\n"))

  def printPath: Unit = printPath(Holder.path)

object Holder:
  var path: List[Status] = Nil
  def empty = path = Nil
  def add(status: Status) = path = status :: path


enum FaceRotation:
  case NoneRotation, OneFourth, OneHalf, ThreeFourth

export FaceRotation.*

enum DirectionDrift:
  lazy val modulo = DirectionDrift.values.length
  case Aligned, DriftOneFourth, DriftOneHalf, DriftThreeFourth

  def +(faceRotation: FaceRotation) =
    val newOrdinal = (ordinal + faceRotation.ordinal)%modulo
    DirectionDrift.fromOrdinal(newOrdinal)

  def -(faceRotation: FaceRotation) =
    val newOrdinal = (ordinal - faceRotation.ordinal) match
      case value if value < 0 => (value % modulo + modulo)
      case value => value % modulo
    DirectionDrift.fromOrdinal(newOrdinal)

export DirectionDrift.*

enum FaceId:
  case One, Two, Three, Four, Five, Six
  def move(direction: Direction): (FaceId, Direction, DirectionDrift) = VirtualTCube.move(this, direction)

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
        leftUpCorners.find:
          case (_, Position(rowStart, colStart)) => row >= rowStart && row < rowStart + faceSize && col >= colStart && col < colStart + faceSize
        .map(_._1).get
    val relativePosition =
      leftUpCorners.filter(_._1 == face).map:
        case (_, Position(rowStart, colStart)) => Position(position.row - rowStart, position.col - colStart)
      .head
    (face, relativePosition)

  def locate(faceTo: FaceId, newDirection: Direction, originalDirection: Direction, relativePosition: Position, drift: DirectionDrift, useRowInformation: Boolean): Position =
    def getRelativePositionMeaningFull =
      useRowInformation match
        case true => relativePosition.row
        case false => relativePosition.col
    def getRelativePositionDrifted =
      useRowInformation match
        case true => faceSize - 1 - relativePosition.row
        case false => faceSize - 1 - relativePosition.col
    def guessRow: Int =
      (drift, useRowInformation, originalDirection) match
        case (Aligned, true, _) | (DriftOneFourth, false, _) | (DriftOneHalf, false, _) | (DriftThreeFourth, true, Left) => getRelativePositionMeaningFull
        case _ => getRelativePositionDrifted
    def guessCol: Int =
      (drift, useRowInformation, originalDirection) match
        case (Aligned, false, _) | (DriftThreeFourth, true, _) | (DriftOneFourth, true, _) | (DriftOneHalf, true, _) => getRelativePositionMeaningFull
        case _ => getRelativePositionDrifted
    val leftUpCorner = leftUpCorners.filter(_._1 == faceTo).map(_._2).head
    newDirection match
      case Up => Position(leftUpCorner.row + faceSize - 1, leftUpCorner.col + guessCol)
      case Down => Position(leftUpCorner.row, leftUpCorner.col + guessCol)
      case Left => Position(leftUpCorner.row + guessRow, leftUpCorner.col + faceSize - 1)
      case Right => Position(leftUpCorner.row + guessRow, leftUpCorner.col)
  def move(position: Position, direction: Direction, undefinedPosition: Position): (Position, Direction) =
    val (faceFrom, relativePosition) = face(position)
    loggerAOCPart2.trace(s"\n\nI am on face n° ${faceFrom} at location ${relativePosition}")
    val (faceTo, newDirection, drift) = canvas.move(faceFrom, direction)
    loggerAOCPart2.trace(s"Supposed to move to face n° ${faceTo} going $newDirection, drift is ${drift}")
    val useRowInformation =
      undefinedPosition - position match
        case (0, _) => true
        case (_, 0) => false
        case _ => throw Exception("Unexpected result")
    val newLocation = locate(faceTo, newDirection, direction, relativePosition, drift, useRowInformation)
    loggerAOCPart2.debug(s"\n\nJump \t${faceFrom}\t${faceTo}\t\t: $direction >= $newDirection - ${relativePosition} to ${face(newLocation)._2}, drift is $drift, we used row ? $useRowInformation")
    loggerAOCPart2.trace(s"Location found is $newLocation [rel : ${face(newLocation)._2}], we used row ? $useRowInformation\n")
    (newLocation, newDirection)

class CubeCanvas(val rTwo: FaceRotation, val rThree: FaceRotation, val rFour: FaceRotation, val rFive: FaceRotation, val rSix: FaceRotation):
  def move(faceFrom: FaceId, direction: Direction): (FaceId, Direction, DirectionDrift) =
    val (faceTo, temporaryDirection, drift) = faceFrom.move(transformOut(faceFrom, direction))
    loggerAOCPart2.trace(s"Drift : $drift => ${drift + rotationFactor(faceTo) - rotationFactor(faceFrom)}")
    (faceTo, transformIn(faceTo, temporaryDirection), drift + rotationFactor(faceTo) - rotationFactor(faceFrom))
  def rotationFactor(faceId: FaceId) = faceId match
    case One => NoneRotation
    case Two => rTwo
    case Three => rThree
    case Four => rFour
    case Five => rFive
    case Six => rSix
  def transformOut(faceFrom: FaceId, direction: Direction): Direction = direction + rotationFactor(faceFrom)

  def transformIn(faceTo: FaceId, direction: Direction): Direction = direction - rotationFactor(faceTo)

object VirtualTCube:
  def move(faceFrom: FaceId, direction: Direction): (FaceId, Direction, DirectionDrift) =
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
    val (next, mustTurn, drift) = faceFrom match
      case One => direction match
        case Up =>      (Four,   false, Aligned)
        case Right =>   (Six,   false, Aligned)
        case Down =>    (Two, false, Aligned)
        case Left =>    (Five,  false, Aligned)
      case Two => direction match
        case Up =>      (One,   false, Aligned)
        case Right =>   (Six,   true, DriftOneFourth)
        case Down =>    (Three, false, Aligned)
        case Left =>    (Five,  true, DriftThreeFourth)
      case Three => direction match
        case Up =>      (Two,   false, Aligned)
        case Right =>   (Six,   true, DriftOneHalf) // => Left
        case Down =>    (Four,  false, Aligned)
        case Left =>    (Five,  true, DriftOneHalf) // => Right
      case Four => direction match
        case Up =>      (Three, false, Aligned)
        case Right =>   (Six,   true, DriftOneFourth)
        case Down =>    (One,   false, DriftOneHalf)
        case Left =>    (Five,  true, DriftOneFourth)
      case Five => direction match
        case Up =>      (Four,  true, DriftThreeFourth)
        case Right =>   (One,   false, Aligned)
        case Down =>    (Two,   true, DriftOneFourth)
        case Left =>    (Three, true, DriftOneHalf)
      case Six => direction match
        case Up =>      (Four,  true, DriftOneFourth)
        case Right =>   (Three, true, DriftOneHalf)
        case Down =>    (Two,   true, DriftThreeFourth)
        case Left =>    (One,   false, Aligned)
    val result = mustTurn match
      case false => sameDir(next)
      case true =>  altDir(next)
    (result._1, result._2, drift)