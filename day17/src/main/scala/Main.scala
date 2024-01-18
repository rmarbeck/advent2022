import Position.moveLeft
import Rock.{Corner, Horizontal, Plus, Square, Vertical}

import scala.io.Source
import com.typesafe.scalalogging.Logger

val loggerAOC = Logger("aoc")
val loggerAOCPart1 = Logger("aoc.part1")
val loggerAOCPart2 = Logger("aoc.part2")

export Move._

@main def hello: Unit =
  loggerAOC.trace("Root trace activated")
  loggerAOC.debug("Root debug activated")
  println("Launching Day17")
  List[() => (String, String)]( () => Solver.solveTest, () => Solver.solve).foreach: f =>
    val (score1, score2) = f.apply()
    println(s"1 : ${score1}")
    println(s"2 : ${score2}")
    println(s"----------------")
  println("Done")

object Solver:
  def runOn(inputLines: Seq[String]): (String, String) =
    val result1 = s""
    val result2 = s""

    //val origin = Position(0,0)
    //println(buildMovesRoundRobin(inputLines(0)).take(140).mkString("-"))
    //println(buildRocksRoundRobin()(0).shape(origin))

    //val chamber2 = Chamber(List(Shape(Position(10,2), Plus)))(7)
    //println(s"${chamber2.asSentence}")
    val movesAsString = inputLines(0)
    val lengthOfWind = movesAsString.length
    val moves = buildMovesRoundRobin(movesAsString).iterator
    val finalChamber = buildRocksRoundRobin().take(2022).foldLeft(Chamber(Nil)(7)):
      case (acc, newRock) => acc.dropRock(newRock, moves)

    //println(finalChamber.zipWithIndex.filter(_._2%1000==0).map(_._1.highestPoint).sliding(2,1).map(current => current(1) - current(0)).mkString("-"))
    println(finalChamber.highestPoint)
    println(findCycle(finalChamber.asSentence.reverse, 5))

    /*given Chamber = chamber2
    val chamber3 = chamber2.addShape(Horizontal(0,2))

    println(Horizontal(Position(0, 2)).canGoDown)
    println(Horizontal(Position(1, 2)).canGoDown)
    println(Horizontal(Position(1, 2)).canGoDown(using chamber3))
    println(Horizontal(Position(1, 2)).canGoLeft)
    println(Vertical(6, 0).canGoLeft)
    println(Vertical(6, 0).canGoRight)*/

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

enum Move:
  case Left, Right

enum Rock:
  def shape(position: Position) = Shape(position, this)
  case Horizontal, Plus, Corner, Vertical, Square

object Move:
  def from(char: Char): Move =
    char match
      case '>' => Right
      case '<' => Left
      case _ => throw Exception("Not managed")

case class Chamber(shapes: List[Shape])(width: Int = 7):
  lazy val asSentence: String =
    def lineToChar(values: Array[Boolean]): Array[Char] =
      val List(char1, char2) = values.splitAt(4) match
        case (start, end) => List(start, end).map(_.zipWithIndex.foldLeft(65):
          case (acc, (value, index)) =>
            val result = value match
              case true => acc + (math.pow(2, index).toInt)
              case false => acc
            result
        .toChar)
      Array(char1, char2)
    data.filterNot(_.count(_ == false) == 0).flatMap(lineToChar(_)).mkString
  def dropRock(newRock: Rock, moves: Iterator[Move]): Chamber =
    def findFinalPositionOfShape(shape: Shape): Shape =
      val currentMove = moves.next()
      //println(this.addShape(shape))
      val newShape = currentMove match
        case Left => shape.moveLeft
        case Right => shape.moveRight
      //println(this.addShape(newShape))
      //println(s"$currentMove ${newShape != shape}")
      newShape.canGoDown match
        case true => findFinalPositionOfShape(newShape.moveDown)
        case false => newShape.moveDown

    given Chamber = this
    this.addShape(findFinalPositionOfShape(newRock.shape(Position(this.highestPoint + 3, 2))))

  def addShape(newShape: Shape): Chamber = Chamber(newShape :: shapes)(width)
  def isFree(position: Position): Boolean =
    data.isDefinedAt(position.row) && data(position.row).isDefinedAt(position.col) match
      case true => data(position.row)(position.col)
      case false => false
  val highestPoint: Int =
    shapes.map(_.positions.map(_.row).max).maxOption match
      case Some(value) => value + 1
      case None => 0
  private val data: Array[Array[Boolean]] = Array.fill(highestPoint + 7, width)(true)
  shapes.foreach:
    case shape: Shape => shape.positions.foreach:
      case position: Position => data(position.row)(position.col) = false


  private def asString(data: Array[Array[Char]]) =
    data.map(_.mkString(" ")).mkString("\n")

  private def toDisplay(boolean: Boolean): Char =
    boolean match
      case true => '.'
      case _ => '#'

  override def toString =
    asString(data.reverse.map(_.map(toDisplay).map(_.toString.head)))

def buildMovesRoundRobin(movesAsString: String, position: Int = 0): LazyList[Move] = Move.from(movesAsString.charAt(position%movesAsString.length)) #:: buildMovesRoundRobin(movesAsString, position+1)
def buildRocksRoundRobin(position: Int = 0): LazyList[Rock] = Rock.fromOrdinal(position%Rock.values.length) #:: buildRocksRoundRobin(position+1)

import Position._
case class Position(row: Int, col: Int):
  private def doNTimes(n: Int)(func: Position => Position) = (1 to n).foldLeft(this)((acc, newVal) => func(acc))
  def xLeft(x: Int): Position = doNTimes(x)(moveLeft)
  def xRight(x: Int): Position = doNTimes(x)(moveRight)
  def xDown(x: Int): Position = doNTimes(x)(moveDown)
  def xUp(x: Int): Position = doNTimes(x)(moveUp)

object Position:
  def moveUp(position: Position): Position = position.copy(row = position.row + 1)
  def moveDown(position: Position): Position = position.copy(row = position.row - 1)
  def moveLeft(position: Position): Position = position.copy(col = position.col - 1)
  def moveRight(position: Position): Position = position.copy(col = position.col + 1)

case class Shape(downLeftCorner: Position, rock: Rock):
  lazy val positions: List[Position] =
    rock match
      case Horizontal => List(downLeftCorner.xUp(2).xRight(1), downLeftCorner.xUp(1), downLeftCorner.xUp(1).xRight(1), downLeftCorner.xUp(1).xRight(2), downLeftCorner.xRight(1))
      case Plus => List(downLeftCorner.xUp(2).xRight(1), downLeftCorner.xUp(1), downLeftCorner.xUp(1).xRight(1), downLeftCorner.xUp(1).xRight(2), downLeftCorner.xRight(1))
      case Corner => List(downLeftCorner.xUp(2).xRight(2), downLeftCorner.xUp(1).xRight(2), downLeftCorner, downLeftCorner.xRight(1), downLeftCorner.xRight(2))
      case Vertical => List(downLeftCorner.xUp(2).xRight(2), downLeftCorner.xUp(1).xRight(2), downLeftCorner, downLeftCorner.xRight(1), downLeftCorner.xRight(2))
      case Square => List(downLeftCorner.xUp(1), downLeftCorner, downLeftCorner.xUp(1).xRight(1), downLeftCorner.xRight(1))
  private final def canMove(direction: Position => Position)(using chamber: Chamber): Boolean = ! positions.map(direction).find(chamber.isFree(_) == false).isDefined

  final def canGoDown(using Chamber): Boolean = canMove(Position.moveDown)
  final def canGoLeft(using Chamber): Boolean = canMove(Position.moveLeft)
  final def canGoRight(using Chamber): Boolean = canMove(Position.moveRight)

  private final def moveIfPossible(direction: Position => Position)(using chamber: Chamber) =
    canMove(direction) match
      case true => this.copy(downLeftCorner = direction.apply(downLeftCorner))
      case false => this

  final def moveDown(using Chamber): Shape = moveIfPossible(Position.moveDown)
  final def moveLeft(using Chamber): Shape = moveIfPossible(Position.moveLeft)
  final def moveRight(using Chamber): Shape = moveIfPossible(Position.moveRight)


def findCycle(toLookIn: String, factor: Int): Int =
  def search(toLookIn: String, current: Int, best: Int): Int =
    val (pattern, tail) = toLookIn.splitAt(current)
    tail.indexOf(pattern) match
      case -1 => best
      case 0 => current
      case value => search(toLookIn, current + factor, current)
  search(toLookIn, factor, 0)