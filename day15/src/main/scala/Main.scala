import scala.io.Source
import com.typesafe.scalalogging.Logger

val loggerAOC = Logger("aoc")
val loggerAOCPart1 = Logger("aoc.part1")
val loggerAOCPart2 = Logger("aoc.part2")

@main def hello: Unit =
  loggerAOC.trace("Root trace activated")
  loggerAOC.debug("Root debug activated")
  println("Launching Day15")
  List[() => (String, String)]( () => Solver.solveTest, () => Solver.solve).foreach: f =>
    val (score1, score2) = f.apply()
    println(s"1 : ${score1}")
    println(s"2 : ${score2}")
    println(s"----------------")
  println("Done")

type IRange = Range.Inclusive

object Solver:
  def runOn(inputLines: Seq[String]): (String, String) =

    val sensors = inputLines.map:
      case s"Sensor at x=$sensorX, y=$sensorY: closest beacon is at x=$beaconX, y=$beaconY" => Sensor.fromStrings(List(sensorX, sensorY, beaconX, beaconY))

    val row = sensors.length match
      case 14 => 10
      case _ => 2000000

    //val underSurveillance = sensors.flatMap(_.impossiblePositionsOnRow(row)).toSet

    val result = sensors.tail.foldLeft((sensors.head.impossiblePositionsOnRow(row), Set[Int]() ++ sensors.head.xPosIfClosestBeaconOnRow(row))) { (acc, sensor) =>
      (merge(acc._1, sensor.impossiblePositionsOnRow(row)), acc._2 ++ sensor.xPosIfClosestBeaconOnRow(row))
    } match
      case (scanned, beacons) =>
        (scanned.end - scanned.start + 1)- beacons.size

    //val presentBeacons = sensors.flatMap(_.xPosIfClosestBeaconOnRow(row)).toSet
    /*val result = sensors.foldLeft(underSurveillance2) { (acc, newValue) =>
      underSurveillance2
    }*/

    //println(presentBeacons)
    //println(underSurveillance)

    //val result = underSurveillance2.filterNot(presentBeacons.contains(_))

    val result1 = s"${result}"
    val result2 = s""

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

def merge(range1: IRange, range2: IRange): IRange = math.min(range1.start, range2.start) to math.max(range1.end, range2.end)

case class Sensor(x: Int, y: Int, closestBeacon: Beacon):
  def impossiblePositionsOnRow(row: Int): IRange =
    val distanceToClosest = math.abs(x - closestBeacon.x) + math.abs(y - closestBeacon.y)
    val shortestDistanceToRow = math.abs(y - row)
    shortestDistanceToRow match
      case value if value <= distanceToClosest => x - (distanceToClosest - value) to x + (distanceToClosest - value)
      case _ => 0 to 0

  def xPosIfClosestBeaconOnRow(row: Int): Option[Int] =
    closestBeacon.y == row match
      case true => Some(closestBeacon.x)
      case false => None

object Sensor:
  def fromStrings(values: List[String]) = values.map(_.toInt) match
    case List(x, y, bX, bY) => Sensor(x, y, Beacon(bX, bY))
    case _ => throw Exception("Unhandled")


case class Beacon(x: Int, y: Int)