import scala.io.Source
import scala.math._

import java.time.Duration
import java.time.Instant

@main def hello: Unit =
  val startTime = Instant.now()
  println("Launching 4-12")
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
    val bufferedSource = Source.fromFile("./src/main/resources/" + fileName)
    val lines = bufferedSource.getLines().toList
    bufferedSource.close

    val result = lines.map: line =>
      line.split(Array('-', ',')).map(_.toInt) match
        case Array(a, b, c, d) => (a, b, c, d) match
          case tuple => (part1.tupled(tuple), part2.tupled(tuple))

    val result1 = result.count((first, _) => first)
    val result2 = result.count((first, second) => first || second)

    (s"${result1}", s"${result2}")


def part1(a: Int, b: Int, c: Int, d: Int): Boolean =
  def contain(fi: Int, se: Int, th: Int, fo: Int) =
    fi to se containsSlice (th to fo)
  contain(a, b, c, d) || contain(c, d, a, b)


def part2(a: Int, b: Int, c: Int, d: Int): Boolean =
  def shareElements(fi: Int, se: Int, th: Int, fo: Int) =
    (fi to se intersect (th to fo)).length != 0
  shareElements(a, b, c, d) || shareElements(c, d, a, b)
