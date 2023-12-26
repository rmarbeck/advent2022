import scala.io.Source
import scala.math._

@main def hello: Unit =
  println("Launching 1-12-1")
  val bufferedSource = Source.fromFile("./src/main/resources/test1.txt")
  val lines = bufferedSource.getLines().toList
  given Ordering[Int] = Ordering.Int.reverse
  val result = lines.foldLeft(List[Int](0)) { (acc, newValue) =>
    newValue match
      case "" => 0 +: acc
      case value => (acc.head + value.toInt) +: acc.tail
  }.sorted.take(3)
  println(s"Result 1 is ${result.head}")
  println(s"Result 2 is ${result.sum}")
  bufferedSource.close
  println("Done")
