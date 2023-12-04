import scala.io.Source
import scala.math._

@main def hello: Unit =
  println("Launching 4-12")
  val bufferedSource = Source.fromFile("./src/main/resources/test1.txt")
  val data = bufferedSource.getLines().toSeq
  val result1 = data.map:
      case s"$a-$b,$c-$d" => (part1(a.toInt, b.toInt, c.toInt, d.toInt), part2(a.toInt, b.toInt, c.toInt, d.toInt))
  println(s"1 : ${result1.filter((first, second) => first == true).map((_,_) => 1).sum}")
  println(s"2 : ${result1.filter((first, second) => first == true || second == true).map((_,_) => 1).sum}")
  //println(s"1 : ${result2.sum}")
  bufferedSource.close
  println("Done")

def part1(a: Int, b: Int, c: Int, d: Int): Boolean =
  (a >= c && b <= d) || (a <= c && d <= b)

def part2(a: Int, b: Int, c: Int, d: Int): Boolean =
  (a == c || b == d) || (a > c && d >= a) || (a < c && b >= c )

def charToItsIntValue(character: Char): Int =
  if (character.isLower)
    return character.toInt - 96
  character.toInt - 38

def commonChar(part1: String, part2: String, part3: String): Char =
  part1.filter(part2.contains(_)).filter(part3.contains(_)).head