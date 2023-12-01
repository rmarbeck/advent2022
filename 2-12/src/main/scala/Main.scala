import scala.io.Source
import scala.math._

@main def hello: Unit =
  println("Launching 1-12-1")
  val bufferedSource = Source.fromFile("./src/main/resources/test1.txt")
  val result = bufferedSource.getLines.map(calculateScore(_)).sum
  println(s"Result is ${result}")
  bufferedSource.close
  println("Done")

def calculateScore(played: String): Int =
  def score2Is(first: Char, second: Char): Int =
    first match
      case 'A' => second match // Pierre
        case 'X' => 3 // Ciseaux 0 + 3
        case 'Y' => 4 // Pierre 3 + 1
        case _ => 8 // Feuille 6 + 2
      case 'B' => second match // Feuille
        case 'X' => 1 // Pierre 0 + 1
        case 'Y' => 5 // Feuille 3 + 2
        case _ => 9 // Ciseaux 6 + 3
      case _ => second match // Ciseaux
        case 'X' => 2 // Feuille 0 + 2
        case 'Y' => 6 // Ciseaux 3 + 3
        case _ => 7 // Pierre 6 + 1
  def score1Is(first: Char, second: Char): Int =
    first match
      case 'A' => second match // Pierre
        case 'X' => 4 // Pierre 1 + 3
        case 'Y' => 8 // Feuille 2 + 6
        case _ => 3  // Ciseaux 3 + 0
      case 'B' => second match // Feuille
        case 'X' => 1 // Pierre 1 + 0
        case 'Y' => 5 // Feuille 2 + 3
        case _ => 9 // Ciseaux 3 + 6
      case _ => second match // Ciseaux
        case 'X' => 7 // Pierre 1 + 6
        case 'Y' => 2 // Feuille 2 + 0
        case _ => 6 // Ciseaux 3 + 3
  score2Is(played.head, played.reverse.head)
