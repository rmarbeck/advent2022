import scala.io.Source
import scala.math._

@main def hello: Unit =
  println("Launching 1-12-1")
  val bufferedSource = Source.fromFile("./src/main/resources/test1.txt")
  val result = bufferedSource.getLines().map(value =>
    value match
      case "" => ";0"
      case number => number
  ).mkString("+").split(";").map(_.split('+').map(_.toInt).sum).sorted.reverse.take(3)
  println(s"Result 1 is ${result.head}")
  println(s"Result 2 is ${result.sum}")
  bufferedSource.close
  println("Done")
