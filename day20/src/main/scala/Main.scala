import scala.io.Source
import com.typesafe.scalalogging.Logger

import scala.annotation.tailrec

val loggerAOC = Logger("aoc")
val loggerAOCPart1 = Logger("aoc.part1")
val loggerAOCPart2 = Logger("aoc.part2")

@main def hello: Unit =
  loggerAOC.trace("Root trace activated")
  loggerAOC.debug("Root debug activated")
  println("Launching Day20")
  List[() => (String, String)]( () => Solver.solveTest, () => Solver.solve).foreach: f =>
    val (score1, score2) = f.apply()
    println(s"1 : ${score1}")
    println(s"2 : ${score2}")
    println(s"----------------")
  println("Done")

object Solver:
  val (startOfScoreIndex, endOfScoreIndex, scoreIndexIncrement) = (1000, 3000, 1000)
  val (part2MultiplyBy, part2Rounds) = (811589153, 10)
  def mix(indexesOrdered: IndexesOrdered, initialNumbers: Seq[Long]) =
    initialNumbers.zipWithIndex.foldLeft(indexesOrdered):
      case (acc, (newValue, index)) => acc.move(index, newValue)

  def extractScore(indexOfZero: Int, sizeOfArray: Int, initialNumbers: Seq[Long], result: IndexesOrdered) = (startOfScoreIndex to endOfScoreIndex by scoreIndexIncrement).map { current =>
    val indexToLookIn = (current + indexOfZero) % sizeOfArray
    val intermediateResult = result.applyTo(initialNumbers)((current + indexOfZero) % sizeOfArray)
    loggerAOC.trace(s"Looking at $indexToLookIn => $intermediateResult")
    intermediateResult
  }.sum
  
  def runOn(inputLines: Seq[String]): (String, String) =
    val numbers = inputLines.map(_.toLong)
    val sizeOfArray = numbers.length

    val initialIndex = IndexesOrdered((0 until sizeOfArray).toList)

    val resultPart1Int = mix(initialIndex, numbers)
    val indexOfZeroPart1 = resultPart1Int.applyTo(numbers).indexOf(0)

    loggerAOCPart1.trace(s"zero is at position $indexOfZeroPart1")
    val resultPart1 = extractScore(indexOfZeroPart1, sizeOfArray, numbers, resultPart1Int)

    
    val numbersPart2 = numbers.map(_*part2MultiplyBy)

    val resultPart2Int = (1 to part2Rounds).foldLeft(initialIndex):
      case (acc, newResult) => mix(acc, numbersPart2)
    val indexOfZeroPart2 = resultPart2Int.applyTo(numbersPart2).indexOf(0)

    loggerAOCPart2.trace(s"zero is at position $indexOfZeroPart2")
    val resultPart2 = extractScore(indexOfZeroPart2, sizeOfArray, numbersPart2, resultPart2Int)

    val result1 = s"$resultPart1"
    val result2 = s"$resultPart2"

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

case class IndexesOrdered(values: Seq[Int]):
  val asArray = values.toArray
  val length = asArray.length

  private def moveInternally(nTh: Int, to: Int): IndexesOrdered =
    val valueToMove = asArray(nTh)
    val listWithoutValueToMove = values.filterNot(_ == valueToMove)
    val (before, after) = listWithoutValueToMove.splitAt(to)
    val updatedList = before.concat(valueToMove +: after)
    IndexesOrdered(updatedList)

  def move(index: Int, of: Long): IndexesOrdered =
    of match
      case 0 => this
      case value =>
        val indexOfValueToMove = asArray.indexOf(index)
        val newPosition = indexOfValueToMove + of match
          case value if value < 0 => (value % (length-1) + length - 1).toInt
          case value if value > length - 1 => (value % (length-1)).toInt
          case value => value.toInt

        moveInternally(indexOfValueToMove, newPosition)

  def applyTo(data: Seq[Long]): Seq[Long] = values.map(value => data(value))
