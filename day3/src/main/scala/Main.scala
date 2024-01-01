import scala.annotation.tailrec
import scala.io.Source
import scala.math.*

import java.time.Duration
import java.time.Instant

@main def hello: Unit =
  val startTime = Instant.now()
  println("Launching 3-12")
  List[() => (String, String)]( () => Solver.solveTest, () => Solver.solve).foreach: f =>
    val (score1, score2) = f.apply()
    println(s"1 : ${score1}")
    println(s"2 : ${score2}")
    println(s"----------------")
  println("Done")
  println(s"Computing time is ${Duration.between(startTime, Instant.now()).toMillis}ms")

object Solver:
  def solveTest: (String, String) =
    solver("test.txt")
  def solve: (String, String) =
    solver("data.txt")
  private def solver(fileName: String): (String, String) =
    val bufferedSource = Source.fromResource(fileName)
    val lines = bufferedSource.getLines().toList
    bufferedSource.close

    def groupAndSumChar(groupSize: Int, data: List[String]): Int =
      data.sliding(groupSize, groupSize).foldLeft(0):
        case (acc, newValue) => acc + commonChar(newValue).getValue

    val result1 = groupAndSumChar(2, lines.flatMap: currentLine =>
      val (first, second) = currentLine.splitAt(currentLine.length / 2)
      List(first,second)
    )

    val result2 = groupAndSumChar(3, lines)


    (s"${result1}", s"${result2}")

class ValuedChar(character: Char):
  //val rankedChars1 = ('A' to 'z').filter(_.isLetter).partition(_.isLower) match
  //  case (lower, upper) => lower.concat(upper).mkString
  private val rankedChars = ('a' to 'z') ++ ('A' to 'Z').mkString
  def getValue: Int =
    rankedChars.indexOf(character) + 1

def commonChar(values: Seq[String]): ValuedChar =
  @tailrec
  def findCommon(values: Seq[String], found: Seq[Char]): Option[Char] =
    values match
      case Nil => found.headOption
      case _ => findCommon(values.tail, found.filter(values.head.contains(_)))
  findCommon(values.tail, values.head) match
    case Some(value) => ValuedChar(value)
    case _ => throw Exception("Not supported")