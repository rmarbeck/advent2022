import scala.io.Source
import com.typesafe.scalalogging.Logger

import scala.annotation.tailrec

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

val multiplier = 5

object Solver:
  def runOn(inputLines: Seq[String]): (String, String) =

    val result = inputLines.map:
      case line => fromSNAFU(line)
    .sum

    val result1 = s"${toSNAFUAsString(result)}"
    val result2 = s"Finished"

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
  val asListOfLong = value.map:
    case '=' => -2
    case '-' => -1
    case value => value.asDigit
  fromSNAFU(asListOfLong.map(_.toLong))

def fromSNAFU(values: Seq[Long]): Long =
  values.foldLeft(0l):
    case (acc, newDigit) => acc*multiplier + newDigit

def toSNAFU(number: Long): List[Long] =
  (number / multiplier, number % multiplier) match
    case (div, mod) if div <= 2 && mod <= 2 => List(div, mod)
    case (div, mod) if div <= 1 && mod > 2 => List(div + 1, mod - multiplier)
    case (div, mod) if div > 2 && mod <= 2 => toSNAFU(div) ::: List(mod)
    case (div, mod) => toSNAFU((number - (mod - multiplier)) / multiplier ) ::: List(mod - multiplier)

@tailrec
def toSNAFURec(number: Long, currentList: List[Long] = Nil): List[Long] =
  (number / multiplier, number % multiplier) match
    case (div, mod) if div <= 2 && mod <= 2 => List(div, mod) ::: currentList
    case (div, mod) if div <= 1 && mod > 2 => List(div + 1, mod - multiplier) ::: currentList
    case (div, mod) if div > 2 && mod <= 2 => toSNAFURec(div, mod :: currentList)
    case (div, mod) => toSNAFURec((number - (mod - multiplier)) / multiplier, (mod - multiplier) :: currentList )


def toSNAFUAsString(number: Long): String =
  toSNAFURec(number).map:
    case value if value == -1 => '-'
    case value if value == -2 => '='
    case value => value.toString
  .mkString