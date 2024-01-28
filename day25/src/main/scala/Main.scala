import scala.io.Source
import com.typesafe.scalalogging.Logger

val loggerAOC = Logger("aoc")
val loggerAOCPart1 = Logger("aoc.part1")
val loggerAOCPart2 = Logger("aoc.part2")

@main def hello: Unit =
  loggerAOC.trace("Root trace activated")
  loggerAOC.debug("Root debug activated")
  println("Launching Day25")
  List[() => (String, String)]( () => Solver.solveTest, () => Solver.solve).foreach: f =>
    val (score1, score2) = f.apply()
    println(s"1 : ${score1}")
    println(s"2 : ${score2}")
    println(s"----------------")
  println("Done")

object Solver:
  def runOn(inputLines: Seq[String]): (String, String) =

    val numberList = inputLines.map:
      case characters => characters.map:
        case '=' => -2
        case '-' => -1
        case value => value.asDigit

    val result = numberList.map:
      case currentList => currentList.foldLeft(0l):
        case (acc, newDigit) => acc*5 + newDigit
    .sum
    
    println(result)


    val result1 = s"${toSNAFUAsString(result)}"
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

def fromSNAFU(value: String): Long =
  value.foldLeft(0l):
    case (acc, newDigit) => acc*5 + newDigit

def fromSNAFU(values: List[Long]): Long =
  values.foldLeft(0l):
    case (acc, newDigit) => acc*5 + newDigit

def toSNAFU(number: Long): List[Long] =
  (number / 5, number % 5) match
    case (div, mod) if div <= 2 && mod <= 2 => List(div, mod)
    case (div, mod) if div <= 1 && mod > 2 => List(div + 1, mod - 5)
    //case (div, mod) if div == 2 && mod > 2 => toSNAFU((number - (mod - 5)) / 5 ) ::: List(mod - 5)
    case (div, mod) if div <= 2 => toSNAFU((number - (mod - 5)) / 5 ) ::: List(mod - 5)
    case (div, mod) if div > 2 && mod <= 2 => toSNAFU(div) ::: List(mod)
    case (div, mod) if div > 2 => toSNAFU((number - (mod - 5)) / 5) ::: List(mod - 5)

def toSNAFUAsString(number: Long): String =
  toSNAFU(number).map:
    case value if value == -1 => '-'
    case value if value == -2 => '='
    case value => value.toString
  .mkString