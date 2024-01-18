import scala.io.Source
import com.typesafe.scalalogging.Logger
import scala.collection.parallel.CollectionConverters._


val loggerAOC = Logger("aoc")
val loggerAOCPart1 = Logger("aoc.part1")
val loggerAOCPart2 = Logger("aoc.part2")

@main def hello: Unit =
  loggerAOC.trace("Root trace activated")
  loggerAOC.debug("Root debug activated")
  println("Launching Day18")
  List[() => (String, String)]( () => Solver.solveTest, () => Solver.solve).foreach: f =>
    val (score1, score2) = f.apply()
    println(s"1 : ${score1}")
    println(s"2 : ${score2}")
    println(s"----------------")
  println("Done")

object Solver:
  def runOn(inputLines: Seq[String]): (String, String) =

    val cubes = inputLines.map:
      case s"$x,$y,$z" => Cube(x.toInt, y.toInt, z.toInt)

    val resultPart1 = cubes.par.map(currentCube => 6 - cubes.filterNot(_ == currentCube).count(_.touches(currentCube))).sum

    val (minX, maxX) = cubes.map(_.x).sorted.splitAt(1) match
      case (list1, list2) => (list1.head, list2.last)
    val (minY, maxY) = cubes.map(_.y).sorted.splitAt(1) match
      case (list1, list2) => (list1.head, list2.last)
    val (minZ, maxZ) = cubes.map(_.z).sorted.splitAt(1) match
      case (list1, list2) => (list1.head, list2.last)

    val cubes2 =
      for x <- minX + 1 until maxX
        y <- minY + 1 until maxY
        z <- minZ + 1 until maxZ
        if Cube(x, y, z).isSurrounded(cubes.toList)
      yield
        Cube(x, y, z)

    //println(cubes2.length)
    val extendedCubes = cubes ++: cubes2
    //println(extendedCubes.length)

    val resultPart2 = cubes.par.map(currentCube => 6 - extendedCubes.filterNot(_ == currentCube).count(_.touches(currentCube))).sum

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

enum Axe:
  case X, Y, Z

export Axe._

case class Cube(x: Int, y: Int, z: Int):
  def isSurrounded(others: List[Cube]): Boolean =
    def isSurroundedOnAxe(axe: Axe): Boolean =
      def test(filters: List[Cube => Int], extractor: Cube => Int) =
        others.filterNot(currentCube => filters.exists(filter => filter(currentCube) != filter(this))).map(extractor).sorted.span(_ <= extractor(this)) match
          case (list1, list2) if list1.isEmpty || list2.isEmpty => false
          case (list1, list2) => list1.last < extractor(this) && list2.head > extractor(this)
      axe match
        case X => test(List(_.y, _.z), _.x)
        case Y => test(List(_.x, _.z), _.y)
        case Z => test(List(_.y, _.x), _.z)

    others.contains(this) match
      case true => false
      case false => isSurroundedOnAxe(X) && isSurroundedOnAxe(Y) && isSurroundedOnAxe(Z)

  def touches(other: Cube): Boolean =
    val diff = other match
        case Cube(oX, oY, oZ) => List(x, y, z, oX, oY, oZ).splitAt(3) match
          case (thisCoords, otherCoords) => thisCoords.zip(otherCoords).map(value => math.abs(value._1 - value._2)).sum
    diff == 1