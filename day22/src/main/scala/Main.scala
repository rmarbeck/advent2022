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

    Holder.empty

    val playground = Playground(playgroundInput)

    println(s"Playgrround : height = ${playground.height}, width = ${playground.width}")

    val path = Path(pathInput.last)

    println(playground)

    def move(status: Status, nextMove: (Int, Turn))(using playground: Playground): Status =
      def next(position: Position, direction: Direction, maxSteps: Int): Position =
        import Position._
        val toGo = direction match
          case Right => goRight
          case Left => goLeft
          case Up => goUp
          case Down => goDown
        playground.nextFree(position, toGo, maxSteps, direction)

      val Status(currentPosition, currentDirection) = status
      val (maxSteps, turn) = nextMove
      val finalDirection = currentDirection.turn(turn)
      val nextPosition = next(currentPosition, currentDirection, maxSteps)
      Status(nextPosition, finalDirection)

    val start = Status(playground.findStart, Right)
    println(start)
    given Playground = playground
    val end = path.moves.foldLeft(start):
      case (acc, newMove) =>
        val result = move(acc, newMove)
        //println(s"$newMove => $result")
        result

    playground.printPath

    println(s" END is $end")
    /*println(end.score)
    println(path)*/

    val result1 = s"${end.score}"
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

enum Direction:
  lazy val modulo = Direction.values.length
  case Right, Down, Left, Up
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

case class Position(row: Int, col: Int)

object Position:
  def goRight(from: Position): Position = from.copy(col = from.col + 1)
  def goLeft(from: Position): Position = from.copy(col = from.col - 1)
  def goDown(from: Position): Position = from.copy(row = from.row + 1)
  def goUp(from: Position): Position = from.copy(row = from.row - 1)

case class Status(position: Position, direction: Direction):
  lazy val score = (position.row + 1) * 1000 + 4 * (position.col + 1) + direction.ordinal

case class Playground(private val input: Seq[String]):
  val width = input.map(_.length).max
  val height = input.size
  val data = Array.fill(height, width)(Empty)
  def isDefined(position: Position) = data.isDefinedAt(position.row) && data(position.row).isDefinedAt(position.col) && data(position.row)(position.col) != Empty
  def isWall(position: Position) = isDefined(position) && data(position.row)(position.col) == SolidWall
  def jumpToNext(undefinedPosition: Position, fromPosition: Position): Position =
    def findMostRightOnRow(row: Int) = Position(row, data(row).zipWithIndex.filter(current => current._1 != Empty).map(_._2).last)
    def findMostLeftOnRow(row: Int) = Position(row, data(row).zipWithIndex.filter(current => current._1 != Empty).map(_._2).head)
    def findMostDownOnCol(col: Int) = Position( (0 until height).filter(data(_)(col) != Empty).last, col)
    def findMostUpOnCol(col: Int) = Position((0 until height).filter(data(_)(col) != Empty).head, col)
    undefinedPosition.row - fromPosition.row match
      case 0 => undefinedPosition.col - fromPosition.col match
        case diffOnCOl if diffOnCOl < 0 => findMostRightOnRow(fromPosition.row)
        case _ => findMostLeftOnRow(fromPosition.row)
      case diffOnRow if diffOnRow < 0 => findMostDownOnCol(fromPosition.col)
      case _ => findMostUpOnCol(fromPosition.col)

  input.zipWithIndex.foreach:
    case (line, index) => line.zipWithIndex.foreach:
      case ('.', indexOfChar) => data(index)(indexOfChar) = Open
      case ('#', indexOfChar) => data(index)(indexOfChar) = SolidWall
      case _ => ()

  def findStart: Position = Position(0, data(0).indexWhere(_ == Open))

  @tailrec
  final def nextFree(fromPosition: Position, walkThrough: Position => Position, maxSteps: Int, direction: Direction): Position =
    Holder.add(Status(fromPosition, direction))
    maxSteps match
      case 0 => fromPosition
      case _ =>
        val newPotentialPosition = walkThrough.apply(fromPosition)
        isDefined(newPotentialPosition) match
          case true => isWall(newPotentialPosition) match
            case true => fromPosition
            case false => nextFree(newPotentialPosition, walkThrough, maxSteps - 1, direction)
          case false =>
            val jumpedPosition = jumpToNext(newPotentialPosition, fromPosition)
            //println(s"Jumped from $fromPosition to $jumpedPosition because of $newPotentialPosition")
            isWall(jumpedPosition) match
              case true => fromPosition
              case false => nextFree(jumpedPosition, walkThrough, maxSteps - 1, direction)

  override def toString: String = data.map(_.mkString).mkString("\n")

  def printPath: Unit =
    for row <- 0 until height
        col <- 0 until width
    do
      Holder.path.find(current => current.position == Position(row, col)) match
        case Some(Status(_, direction)) => direction match
          case Up => print("^")
          case Down => print("v")
          case Right => print(">")
          case Left => print("<")
        case None => print(data(row)(col))
      if (col == width-1)
        println("")

object Holder:
  var path : List[Status] = Nil
  def empty = path = Nil
  def add(status: Status) = path = status :: path