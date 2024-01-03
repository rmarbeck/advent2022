import scala.io.Source
import com.typesafe.scalalogging.Logger

val loggerAOC = Logger("aoc")
val loggerAOCPart1 = Logger("aoc.part1")
val loggerAOCPart2 = Logger("aoc.part2")

@main def hello: Unit =
  loggerAOC.trace("Root trace activated")
  loggerAOC.debug("Root debug activated")
  println("Launching Day10")
  List[() => (String, String)]( () => Solver.solveTest, () => Solver.solve).foreach: f =>
    val (score1, score2) = f.apply()
    println(s"1 : ${score1}")
    println(s"2 : ${score2}")
    println(s"----------------")
  println("Done")

object Solver:
  val CRTWidth = 40
  val emptySprite = " "
  val fullSprite = "#"

  def runOn(inputLines: Seq[String]): (String, String) =


    val part1 = inputLines.flatMap:
      case s"addx $value" => List(0, value.toInt)
      case s"noop" => List(0)
    .scanLeft(1)(_+_).zipWithIndex.filter((value, index) => (20 to 220 by CRTWidth).contains(index+1)).map((value, index) => value * (index +1)).sum

    val part2 = inputLines.flatMap:
      case s"addx $value" => List(0, value.toInt)
      case s"noop" => List(0)
    .scanLeft(1)(_ + _).zipWithIndex.map((value, index) => (index + 1, List(value-1, value, value+1).contains(index%CRTWidth)))

    //println(part2)

    val result1 = s"$part1"
    val result2 = s"${drawInString(part2.toList, CRTWidth)}"

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

  def drawInString(values: List[(Int, Boolean)], width: Int): String =
    values.map:
      case (idx, bool) if (idx % width == 1) => s"\n${if bool then fullSprite else emptySprite}"
      case (_, true) => fullSprite
      case _ => emptySprite
    .mkString

  def draw(values: List[(Int, Boolean)], width: Int): Unit =
    println(drawInString(values, width))

end Solver


