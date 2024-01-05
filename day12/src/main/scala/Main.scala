import scala.io.Source
import com.typesafe.scalalogging.Logger

val loggerAOC = Logger("aoc")
val loggerAOCPart1 = Logger("aoc.part1")
val loggerAOCPart2 = Logger("aoc.part2")

@main def hello: Unit =
  loggerAOC.trace("Root trace activated")
  loggerAOC.debug("Root debug activated")
  println("Launching Day12")
  List[() => (String, String)]( () => Solver.solveTest, () => Solver.solve).foreach: f =>
    val (score1, score2) = f.apply()
    println(s"1 : ${score1}")
    println(s"2 : ${score2}")
    println(s"----------------")
  println("Done")

object Solver:
  def runOn(inputLines: Seq[String]): (String, String) =

    val container = Container(inputLines)

    val result1 = s"${container.solvePart1}"
    val result2 = s"${container.solvePart2}"

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

class Container(input: Seq[String]):
  val width = input(0).length
  val height: Int = input.length
  private val data: Array[Array[Char]] = input.toArray.map(_.toArray)

  lazy val charsAtoZ = ('a' to 'z').toArray

  def solvePart1 = Dijkstra.solve(asGraph, findStart, List(findEnd))

  def solvePart2 = Dijkstra.solve(asGraphPart2, findEnd, findA.toList)

  def find(char: Char): Summit =
    val row = data.indexWhere(_.contains(char))
    val col = data(row).indexOf(char)
    Summit(row, col, char)

  def findStart = find('S')

  def findEnd = find('E')

  def findA = for row <- 0 until height
                  col <- 0 until width
                  if (data(row)(col) == 'a')
              yield
                Summit(row, col, data(row)(col))

  def asGraph: GraphFromArrayUnWeighted =
    val summits = for row <- 0 until height
        col <- 0 until width yield
      Summit(row, col, data(row)(col))

    given start: Char = 'S'
    given endChars: List[Char] = List('E')
    given ordered: Array[Char] = charsAtoZ
    GraphFromArrayUnWeighted(summits)(next)

  def asGraphPart2: GraphFromArrayUnWeighted =
    val summits = for row <- 0 until height
                      col <- 0 until width yield
      Summit(row, col, data(row)(col))
    given start: Char = 'E'
    given endChars: List[Char] = List('a', 'S')
    given ordered: Array[Char] = charsAtoZ.tail.reverse
    GraphFromArrayUnWeighted(summits)(next)

  def next(summit: Summit)(implicit start: Char, endChars: List[Char], ordered: Array[Char]): Seq[Summit] =
    next(summit.row, summit.col).map((curRow, curCol) => Summit(curRow, curCol, data(curRow)(curCol)))

  def next(row: Int, col: Int)(implicit start: Char, endChars: List[Char], ordered: Array[Char]): Seq[(Int, Int)] =
    val currentChar = data(row)(col)
    Seq((row-1, col), (row+1, col), (row, col-1), (row, col+1)).filter:
      data.isDefinedAt(_) && data(row).isDefinedAt(_)
    .filter:
      data(_)(_) match
        case value if endChars.contains(value) =>
          if currentChar == ordered.last then
            true
          else
            false
        case value if value == ordered.head && currentChar == start => true
        case value if ordered.indexOf(value) <= ordered.indexOf(currentChar) + 1 => true
        case _ => false

  def asString(data: Array[Array[Char]]) =
    data.map(_.mkString(" ")).mkString("\n")

  override def toString =
    asString(data.map(_.map(_.toString.head)))