import scala.io.Source
import scala.math._

import scala.collection.mutable


@main def hello: Unit =
  println("Launching 1-12-1")
  val bufferedSource = Source.fromFile("./src/main/resources/test1.txt")
  var totals: Seq[Int] = Seq()
  var current = 0
  for (line <- bufferedSource.getLines)
    if (line.isEmpty) {
      totals = current+:totals
      current = 0
    } else {
      current = current+line.toInt
    }
  val result = totals.sorted.reverse.take(3).sum
  println(s"Result is ${result}")
  bufferedSource.close
  println("Done")

def extract(lineContent: String): Int =
  def first(lineContent: String): Int =
    lineContent.find(_.isDigit).map(_.asDigit).getOrElse(0)
  first(lineContent)*10+first(lineContent.reverse)

