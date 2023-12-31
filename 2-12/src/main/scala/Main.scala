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
    val bufferedSource = Source.fromResource(fileName)
    val lines = bufferedSource.getLines().toList
    bufferedSource.close
    val (result1, result2) = lines.map(calc).foldLeft((0, 0)):
      case (acc, (fromOne, fromTwo)) => (acc._1 + fromOne, acc._2 + fromTwo)

    (s"${result1}", s"${result2}")


enum PlayResult(val score: Int):
  case Win extends PlayResult(6)
  case Draw extends PlayResult(3)
  case Loss extends PlayResult(0)

enum RPS(val score: Int):
  case Rock extends RPS(1)
  case Paper extends RPS(2)
  case Scissors extends RPS(3)

object RPS:
  def fromChar(char: Char): RPS =
    char match
      case 'A' | 'X' => Rock
      case 'B' | 'Y' => Paper
      case 'C' | 'Z' => Scissors
  def playPart1(rps1: RPS, rps2: RPS): Int =
    import PlayResult._
    def matchEnding: PlayResult =
      (rps1, rps2) match
        case values @ ((Rock, Paper) | (Paper, Rock)) => values._1 match
          case Rock => Win
          case _ => Loss
        case values @ ((Rock, Scissors) | (Scissors, Rock)) => values._1 match
          case Scissors => Win
          case _ => Loss
        case values @ ((Scissors, Paper) | (Paper, Scissors)) => values._1 match
          case Paper => Win
          case _ => Loss
        case _ => Draw

    rps2.score + matchEnding.score

  def playPart2(rps1: RPS, rps2: RPS): Int =
    import PlayResult._
    def matchEnding: PlayResult =
      rps2 match
        case Rock => Loss
        case Paper => Draw
        case Scissors => Win

    val toPlay = (matchEnding, rps1) match
      case (Draw, value) => value
      case (Win, Rock) => Paper
      case (Loss, Rock) => Scissors
      case (Win, Paper) => Scissors
      case (Loss, Paper) => Rock
      case (Win, Scissors) => Rock
      case (Loss, Scissors) => Paper

    toPlay.score + matchEnding.score

def calc(inputLine: String): (Int, Int) =
  (calcPart1(inputLine), calcPart2(inputLine))

def calcPart1(inputLine: String): Int =
  inputLine match
    case s"$first $second" => RPS.playPart1(RPS.fromChar(first.head), RPS.fromChar(second.head))

def calcPart2(inputLine: String): Int =
  inputLine match
    case s"$first $second" => RPS.playPart2(RPS.fromChar(first.head), RPS.fromChar(second.head))