import scala.io.Source
import com.typesafe.scalalogging.Logger

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

val loggerAOC = Logger("aoc")
val loggerAOCPart1 = Logger("aoc.part1")
val loggerAOCPart2 = Logger("aoc.part2")

@main def hello: Unit =
  loggerAOC.trace("Root trace activated")
  loggerAOC.debug("Root debug activated")
  println("Launching Day24")
  List[() => (String, String)]( () => Solver.solveTest, () => Solver.solve).foreach: f =>
    val (score1, score2) = f.apply()
    println(s"1 : ${score1}")
    println(s"2 : ${score2}")
    println(s"----------------")
  println("Done")

object Solver:
  def runOn(inputLines: Seq[String]): (String, String) =

    val (height, width) = (inputLines.length, inputLines(0).length)

    val winds = inputLines.drop(1).dropRight(1).zipWithIndex.flatMap:
      case (characters, row) => characters.zipWithIndex.foldLeft(List[Blizzard]()):
        case (acc, (character, col)) => character match
          case '.' | '#' => acc
          case '<' => Blizzard(Position(row + 1, col), Left) :: acc
          case '>' => Blizzard(Position(row + 1, col), Right) :: acc
          case '^' => Blizzard(Position(row + 1, col), Up) :: acc
          case 'v' => Blizzard(Position(row + 1, col), Down) :: acc

    val valley = Valley(height, width, winds.toList)

    val valleyList = valleys(valley)
    val initialValley = valleyList(0)
    val (start, end) = (initialValley.start, initialValley.end)

    val solPart1 = startSolveValley(Status(initialValley), 0, end, valleyList)

    val endOfPart1Valley = valleyList(solPart1)

    val part2GoBack = startSolveValley(Status(endOfPart1Valley.end, endOfPart1Valley), solPart1, start, valleyList)

    val midOfPart2Valley = valleyList(part2GoBack)

    val solPart2 = startSolveValley(Status(midOfPart2Valley), part2GoBack, end, valleyList)

    val result1 = s"${solPart1}"
    val result2 = s"${solPart2}"

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

class Status(val position: Position, valley: Valley):
  def this(valley: Valley) = this(valley.start, valley)
  val (height, width) = (valley.height, valley.width)
  lazy val cabDistance = (position.row - height - 1) + (position.col - width - 1)


case class Position(row: Int, col: Int)

enum Direction:
  case Up, Right, Down, Left

export Direction.*

case class Blizzard(position: Position, direction: Direction):
  def next(using valley: Valley): Blizzard =
    val newPosition =
      direction match
        case Up => position.row match
          case 1 => position.copy(row = valley.height - 2)
          case value => position.copy(row = value - 1)
        case Down => position.row match
          case value if value == valley.height - 2 => position.copy(row = 1)
          case value => position.copy(row = value + 1)
        case Right => position.col match
          case value if value == valley.width - 2 => position.copy(col = 1)
          case value => position.copy(col = value + 1)
        case Left => position.col match
          case 1 => position.copy(col = valley.width - 2)
          case value => position.copy(col = value - 1)
    this.copy(position = newPosition)

class Valley(val height: Int, val width: Int, val winds: List[Blizzard]):
  def next: Valley = Valley(height, width, winds.map(_.next(using this)))
  val grid: Array[Array[Boolean]] = Array.fill(height, width)(false)
  winds.foreach:
    case Blizzard(Position(row, col), _) => grid(row)(col) = true
  lazy val start: Position = Position(0, 1)
  lazy val end: Position = Position(height - 1, width - 2)
  def possibleNextFrom(position: Position): List[Status] =
    List(position, position.copy(row = position.row - 1), position.copy(row = position.row + 1), position.copy(col = position.col - 1), position.copy(col = position.col + 1))
      .filter:
        case value if value == start || value == end => true
        case Position(0, _) | Position(_, 0) => false
        case Position(row, col) if row < 0 || col <0  => false
        case Position(row, col) if row >= height - 1 || col >= width - 1  => false
        case _ => true
      .filter:
        case Position(row, col) => ! grid(row)(col)
    .map(position => Status(position, this))


object Valley:
  import scala.collection.mutable
  var valleys: mutable.Map[Int, Valley] = mutable.Map[Int, Valley]()
  def start(valley: Valley) = valleys = mutable.Map(0 -> valley)
  def get(minute: Int): Valley =
    valleys.size match
      case value if value >= minute => valleys.get(minute).get
      case value =>
        val newValley = get(minute - 1).next
        valleys.getOrElseUpdate(minute, newValley)

@tailrec
def solveValley(validPositions: List[(Status, Int)], target: Position, valleys: Seq[Valley], best: Int = Int.MaxValue): Int =
  validPositions match
    case Nil => best
    case head :: tail =>
      val (position, minute) = (head._1.position, head._2)
      loggerAOCPart1.trace(s"$position $minute")
      minute + 1 >= best match
        case true => best
        case false =>
          val next = valleys(minute + 1).possibleNextFrom(position)
          next.map(_.position).contains(target) match
            case true => solveValley(tail, target, valleys, minute + 1)
            case false =>
              val newValidPositions = (next.map((_, minute + 1)) ::: tail).distinctBy(current => (current._1.position, current._2)).sortBy(current => (current._1.cabDistance + 100 * current._2, current._1.cabDistance))
              solveValley(newValidPositions, target, valleys, best)

def startSolveValley(startStatus: Status, startMinute: Int, target: Position, valleys: Seq[Valley]): Int =
  solveValley(List((startStatus, startMinute)), target, valleys, Int.MaxValue)

def valleys(fromValley: Valley): LazyList[Valley] = fromValley #:: valleys(fromValley.next)