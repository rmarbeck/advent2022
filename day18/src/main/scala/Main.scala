import scala.io.Source
import com.typesafe.scalalogging.Logger

import scala.annotation.tailrec
import scala.collection.parallel.CollectionConverters.*


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

    val dropletBorders = inputLines.map:
      case s"$x,$y,$z" => Cube(x.toInt, y.toInt, z.toInt)

    val resultPart1 = dropletBorders.par.map(currentCube => 6 - dropletBorders.filterNot(_ == currentCube).count(_.touches(currentCube))).sum

    def rangeOn(onCoord: Cube => Int): Range =
      val (min, max) = dropletBorders.map(onCoord).sorted.splitAt(1) match
        case (list1, list2) => (list1.head, list2.last)
      min to max

    val (cubesPotentiallyInternal, cubesOut) =
      (for x <- rangeOn(_.x)
        y <- rangeOn(_.y)
        z <- rangeOn(_.z)
        if (! dropletBorders.contains(Cube(x, y, z)))
      yield
        (Cube(x, y, z), Cube(x, y, z).isSurrounded(dropletBorders.toList))
        ).partition(_._2 == true) match
          case (first, second) => (first.map(_._1).toList, second.map(_._1).toList)


    val insideDroplet = getInternalCubes(cubesPotentiallyInternal, cubesOut)

    val dropletBordersAndInside = dropletBorders ++: insideDroplet

    val resultPart2 = dropletBorders.par.map(currentCube => 6 - dropletBordersAndInside.filterNot(_ == currentCube).count(_.touches(currentCube))).sum

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
  def coords: List[Int] = List(x, y, z)
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

  def touches(other: Cube): Boolean = this.coords.zip(other.coords).map(value => math.abs(value._1 - value._2)).sum == 1

@tailrec
def getInternalCubes(candidates: List[Cube], externals: List[Cube], internals: List[List[Cube]] = Nil): List[Cube] =
  candidates match
    case Nil => internals.flatten
    case head :: tail =>
      val (connected, unconnected) = internals.partition(currentList => currentList.exists(_.touches(head)))
      val merged = connected.foldLeft(head :: Nil):
        case (acc, newList) => acc ::: newList
      externals.exists(_.touches(head)) match
        case true => getInternalCubes(tail, externals ::: merged, unconnected)
        case false => getInternalCubes(tail, externals, merged :: unconnected)
