import scala.io.Source
import com.typesafe.scalalogging.Logger

val loggerAOC = Logger("aoc")
val loggerAOCPart1 = Logger("aoc.part1")
val loggerAOCPart2 = Logger("aoc.part2")

@main def hello: Unit =
  loggerAOC.trace("Root trace activated")
  loggerAOC.debug("Root debug activated")
  println("Launching Day13")
  List[() => (String, String)]( () => Solver.solveTest, () => Solver.solve).foreach: f =>
    val (score1, score2) = f.apply()
    println(s"1 : ${score1}")
    println(s"2 : ${score2}")
    println(s"----------------")
  println("Done")

object Solver:
  def runOn(inputLines: Seq[String]): (String, String) =
    val result = inputLines.filterNot(_.isEmpty).grouped(2).zipWithIndex.map:
      case (List(part1: String, part2: String), index) => (parse(part1).head.compare(parse(part2).head), index+1)
    .filter((value, index) => value == -1).map(_._2)

    val dividers = Seq("[[2]]", "[[6]]")
    val inputAndDivider = dividers ++: inputLines

    val resultPart2 = inputAndDivider.filterNot(_.isEmpty).map:
      case asString: String => parse(asString).head
    .sorted.zipWithIndex.filter((current, index) => dividers.contains(current.toString)).map(_._2 + 1)

    val result1 = s"${result.sum}"
    val result2 = s"${resultPart2.product}"

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

def parse(input: String): List[Element] =
  input.toList match
    case Nil => List()
    case head :: tail =>
      head match
        case char if char.isDigit => extractInt(input) match
          case value :: Nil => List(Integer(value.toInt))
          case intValue :: rest => Integer(intValue.toInt) +: parse(rest(0))
          case _ => throw Exception("Unsupported")
        case '[' => pop(input) match
          case value :: Nil => List(ListOfIntegers(parse(value)))
          case listValue :: rest => ListOfIntegers(parse(listValue)) +: parse(rest(0))
          case _ => throw Exception("Unsupported")

def extractInt(toAnalyse: String): List[String] =
  toAnalyse.contains(',') match
    case true => toAnalyse.span(_ != ',') match
      case (beginning, end) => List(beginning, end.tail)
    case false => List(toAnalyse)

def pop(toAnalyse: String): List[String] =
  def pop(current: String, deepness: Int, matching: String): List[String] =
    current.head match
      case '[' if deepness == 0 => pop(current.tail, 1, "")
      case value @ '[' => pop(current.tail, deepness+1, matching+value)
      case ']' if deepness == 1 => current.tail match
        case "" => List(matching)
        case _ => List(matching, current.tail.drop(1))
      case value @ ']' => pop(current.tail, deepness-1, matching+value)
      case value => pop(current.tail, deepness, matching+value)
  pop(toAnalyse, 0, "")

sealed trait Element extends Ordered[Element]:
  def compare(other: Element): Int =
    (this, other) match
      case (first: Integer, second: Integer) => first.value.compare(second.value)
      case (firsts: ListOfIntegers, seconds: ListOfIntegers) =>
        firsts.values.zip(seconds.values).map((first, second) => first.compare(second)).filterNot(_ == 0).headOption match
          case Some(value) => value
          case None => firsts.values.length.compare(seconds.values.length)
      case (first: Integer, seconds: ListOfIntegers) => ListOfIntegers(List(first)).compare(seconds)
      case (firsts: ListOfIntegers, second: Integer) => firsts.compare(ListOfIntegers(List(second)))
      case _ => throw Exception("Unsupported")


case class Integer(value: Int) extends Element:
  override def toString: String = s"$value"
case class ListOfIntegers(values: List[Element]) extends Element:
  override def toString: String = s"[${values.map(_.toString).mkString(",")}]"