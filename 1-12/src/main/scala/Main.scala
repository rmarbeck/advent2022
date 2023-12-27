import scala.io.Source
import scala.math._

import java.time.Duration
import java.time.Instant

@main def hello: Unit =
  val startTime = Instant.now()

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

    given Ordering[Int] = Ordering.Int.reverse

    val result = lines.foldLeft(List[Int](0)):
        case (acc, "") => 0 +: acc
        case (acc, value) => (acc.head + value.toInt) +: acc.tail
    .sorted.take(3)

    val (result1, result2) = (s"${result.head}", s"${result.sum}")

    (s"${result1}", s"${result2}")