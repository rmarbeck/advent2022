import scala.io.Source
import com.typesafe.scalalogging.Logger

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

    println(s"$height")

    val result1 = s"${valley.next.next.next.next.next.winds}"
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

case class Valley(height: Int, width: Int, winds: List[Blizzard]):
  def next: Valley = this.copy(winds = winds.map(_.next(using this)))