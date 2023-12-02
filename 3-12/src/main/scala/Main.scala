import scala.io.Source
import scala.math._

@main def hello: Unit =
  println("Launching 2-12")
  val bufferedSource = Source.fromFile("./src/main/resources/test1.txt")
  val data = bufferedSource.getLines().toSeq
  val result1 = data.map(currentLine => currentLine.splitAt(currentLine.length/2)).map((start, end)=> start.find(end.contains(_)).map(charToItsIntValue).getOrElse(0)).toSeq
  val result2 = data.zipWithIndex.map((line, index) => if ((index+1)%3==0) {line+";"} else {line+","}).mkString.split(';').map:
    case s"$part1,$part2,$part3" => commonChar(part1, part2, part3)
  .map(charToItsIntValue).toSeq
  println(s"1 : ${result1.sum}")
  println(s"1 : ${result2.sum}")
  bufferedSource.close
  println("Done")


def charToItsIntValue(character: Char): Int =
  if (character.isLower)
    return character.toInt - 96
  character.toInt - 38

def commonChar(part1: String, part2: String, part3: String): Char =
  part1.filter(part2.contains(_)).filter(part3.contains(_)).head