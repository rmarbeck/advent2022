import scala.io.Source
import com.typesafe.scalalogging.Logger

import scala.annotation.tailrec

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


object Solver:
  def runOn(inputLines: Seq[String]): (String, String) =

    val sensors = inputLines.map:
      case s"Sensor at x=$sensorX, y=$sensorY: closest beacon is at x=$beaconX, y=$beaconY" => Sensor.fromStrings(List(sensorX, sensorY, beaconX, beaconY))

    val row = sensors.length match
      case 14 => 10
      case _ => 2000000

    val rows = sensors.length match
      case 14 => 0 to 20
      case _ => 0 to 4000000

    //val underSurveillance = sensors.flatMap(_.impossiblePositionsOnRow(row)).toSet

    val result = sensors.tail.foldLeft((sensors.head.impossiblePositionsOnRow(row), Set[Int]() ++ sensors.head.xPosIfClosestBeaconOnRow(row))) { (acc, sensor) =>
      (merge(acc._1, sensor.impossiblePositionsOnRow(row)), acc._2 ++ sensor.xPosIfClosestBeaconOnRow(row))
    } match
      case (scanned, beacons) =>
        scanned.size - beacons.size

    val resultPart2 = rows.zipWithIndex.map((currentRow, index) => (sensors.tail.foldLeft(sensors.head.impossiblePositionsOnRow(currentRow)) { (acc, sensor) =>
      merge(acc, sensor.impossiblePositionsOnRow(currentRow))
    }.shrinkTo(rows.start, rows.end), index)).find(_._1.size == rows.end).map((rangeMatching, rowIndex) => (rowIndex, rangeMatching.findHoles(rows.start, rows.end))).map:
      case (rowIndex, SingleRange(start, end)) => rowIndex.toLong + start.toLong * 4000000l
      case _ => throw Exception("Not expected result")
    .getOrElse(0l)

    val result1 = s"${result}"
    val result2 = s"${resultPart2}"

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

def merge(range1: RangeSet, range2: RangeSet): RangeSet = range1.merge(range2)

case class Sensor(x: Int, y: Int, closestBeacon: Beacon):
  def impossiblePositionsOnRow(row: Int): RangeSet =
    val distanceToClosest = math.abs(x - closestBeacon.x) + math.abs(y - closestBeacon.y)
    val shortestDistanceToRow = math.abs(y - row)
    shortestDistanceToRow match
      case value if value <= distanceToClosest => SingleRange(x - (distanceToClosest - value), x + (distanceToClosest - value))
      case _ => CompositeRange(Seq())

  def xPosIfClosestBeaconOnRow(row: Int): Option[Int] =
    closestBeacon.y == row match
      case true => Some(closestBeacon.x)
      case false => None

object Sensor:
  def fromStrings(values: List[String]) = values.map(_.toInt) match
    case List(x, y, bX, bY) => Sensor(x, y, Beacon(bX, bY))
    case _ => throw Exception("Unhandled")


case class Beacon(x: Int, y: Int)


sealed trait RangeSet:
  def merge(other: RangeSet): RangeSet
  def size: Int
  def shrinkTo(min: Int, max: Int): RangeSet
  def findHoles(min: Int, max: Int): RangeSet

case class SingleRange(start: Int, end: Int) extends RangeSet with Ordered[SingleRange]:
  require(start <= end)

  override def findHoles(min: Int, max: Int): RangeSet =
    this.compare(SingleRange(min, max)) match
      case value if value < 0 => SingleRange(this.end, max)
      case value if value > 0 => SingleRange(min, this.start)
      case _ => CompositeRange(Seq())
  override def size: Int = end - start + 1

  override def shrinkTo(min: Int, max: Int): RangeSet = SingleRange(math.max(start, min), math.min(end, max))
  def overlapWith(other: SingleRange): Boolean =
    this.compare(other) match
      case value if value <= 0 => other.start <= this.end
      case _ => other.overlapWith(this)

  override def merge(other: RangeSet): RangeSet =
    other match
      case otherSingle @ SingleRange(otherStart, otherEnd) =>
        this.compare(otherSingle) <= 0 match
          case true => this.end < otherStart match
            case true => CompositeRange(List(this, otherSingle))
            case false => SingleRange(this.start, math.max(this.end, otherEnd))
          case false => other.merge(this)

      case _ => other.merge(this)

  override def compare(that: SingleRange): Int =
    start - that.start match
      case 0 => (end - that.end).sign
      case startDiff => startDiff.sign

case class CompositeRange(singleRanges: Seq[SingleRange]) extends RangeSet:
  private lazy val sorted = singleRanges.sorted
  override def findHoles(min: Int, max: Int): RangeSet =
    (this.shrinkTo(min, max) match
      case single @ SingleRange(start, end) => single.findHoles(min, max)
      case composite @ CompositeRange(values) =>
        CompositeRange(composite.sorted.sliding(2,1).map:
          case Seq(first: SingleRange, second: SingleRange) => SingleRange(first.end + 1, second.start - 1)
        .toSeq)) match
      case CompositeRange(Seq(singleValue)) => singleValue
      case composite => composite



  override def shrinkTo(min: Int, max: Int): RangeSet =
    sorted match
      case Nil => this
      case head :: Nil =>
        head.shrinkTo(min, max)
      case head :: tail =>
        head.shrinkTo(min, max).merge(CompositeRange(tail).shrinkTo(min, max))
  override def size: Int = sorted.map(_.size).sum
  override def merge(other: RangeSet): RangeSet =
    other match
      case otherSingle @ SingleRange(otherStart, otherEnd) =>
        @tailrec
        def merger(remainingRanges: CompositeRange, newRanges: CompositeRange, buildRange: SingleRange): RangeSet =
          remainingRanges.sorted match
            case Nil => CompositeRange(newRanges.sorted ++ Seq(buildRange))
            case _ =>
              val current = remainingRanges.sorted.head
              current.overlapWith(buildRange) match
                case true =>
                  buildRange.merge(current) match
                    case merged @ SingleRange(_, _) => merger(CompositeRange(remainingRanges.sorted.tail), CompositeRange(newRanges.sorted), merged)
                    case _ => throw Exception("Not expected behavior")
                case false =>
                  current.compare(buildRange) match
                    case value if value < 0 => merger(CompositeRange(remainingRanges.sorted.tail), CompositeRange(newRanges.sorted ++ Seq(current)), buildRange)
                    case _ => CompositeRange(newRanges.sorted ++ Seq(buildRange) ++ remainingRanges.sorted)

        merger(this, CompositeRange(Seq()), otherSingle) match
          case CompositeRange(Seq(singleValue)) => singleValue
          case composite => composite
      case CompositeRange(otherRanges) =>
        otherRanges.foldLeft(this: RangeSet) {(acc, newRange) =>
          acc.merge(newRange)
        }