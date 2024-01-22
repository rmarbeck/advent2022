import scala.io.Source
import com.typesafe.scalalogging.Logger

import scala.annotation.tailrec

val loggerAOC = Logger("aoc")
val loggerAOCPart1 = Logger("aoc.part1")
val loggerAOCPart2 = Logger("aoc.part2")

@main def hello: Unit =
  loggerAOC.trace("Root trace activated")
  loggerAOC.debug("Root debug activated")
  println("Launching Day20")
  List[() => (String, String)]( () => Solver.solveTest, () => Solver.solve).foreach: f =>
    val (score1, score2) = f.apply()
    println(s"1 : ${score1}")
    println(s"2 : ${score2}")
    println(s"----------------")
  println("Done")

object Solver:
  def runOn(inputLines: Seq[String]): (String, String) =

    val numbers = inputLines.map(_.toLong)

    //println(numbers)


    val sizeOfArray = numbers.length
    val initialArray = numbers.toArray
    val indexesOrdered = IndexesOrdered((0 until sizeOfArray).map(_.toLong).toList)
    val indexesOrderedStep1 = indexesOrdered.move(0, 1).move(1,2)
    //println(s"0 => $indexesOrdered")
    //println(s"1 => ${indexesOrdered.move(0, 1)}")
    //println(s"2 => ${indexesOrdered.move(0, 1).move(1,2).mixWith(numbers)}")
    //println(s"3 => ${indexesOrdered.move(0, 1).move(1, 2).move(2, -3)   .mixWith(numbers)}")
    val result = numbers.zipWithIndex.foldLeft(indexesOrdered):
      case (acc, (newValue, index)) =>
        val result = acc.move(index, newValue)
        result

    /*val resultb = numbers.zipWithIndex.scanLeft(indexesOrdered):
      case (acc, (newValue, index)) =>
        acc.move(index, newValue)*/

    val indexOfZero = result.mixWith(numbers).indexOf(0)
    println(s"zero is at position $indexOfZero")
    val resultPart1 = (1000 to 3000 by 1000).map { current =>
      println(s"Looking at ${(current+indexOfZero)%sizeOfArray} => ${result.mixWith(numbers)((current+indexOfZero)%sizeOfArray)}")
      result.mixWith(numbers)((current+indexOfZero)%sizeOfArray)
    }.sum
    println(s"$resultPart1")

    val inputPart2 = numbers.map(_*811589153)

    def mixes(indexesOrdered: IndexesOrdered) = inputPart2.zipWithIndex.foldLeft(indexesOrdered):
      case (acc, (newValue, index)) =>
        val result = acc.move(index, newValue)
        result

    val resultPart2Int = (1 to 10).foldLeft(indexesOrdered):
      case (acc, newResult) =>
        val result = mixes(acc)
        //println(acc.mixWith(inputPart2))
        result

    val indexOfZeroPart2 = resultPart2Int.mixWith(inputPart2).indexOf(0)

    println(s"zero is at position $indexOfZeroPart2")
    val resultPart2 = (1000 to 3000 by 1000).map { current =>
      println(s"Looking at ${(current+indexOfZeroPart2)%sizeOfArray} => ${result.mixWith(inputPart2)((current+indexOfZeroPart2)%sizeOfArray)}")
      resultPart2Int.mixWith(inputPart2)((current + indexOfZeroPart2) % sizeOfArray)
    }.sum
    println(s"$resultPart2")

    val result1 = s"$resultPart1"
    val result2 = s"$resultPart2"
    /*val resultPart2 = numbers.zipWithIndex.foldLeft(indexesOrdered):
      case (acc, (newValue, index)) =>
        val result = acc.move(index, newValue)
        result*/


    /*val indexOfZerob = resultb.last.mixWith(numbers).indexOf(0)
    val resultPart1b = (1000 to 3000 by 1000).map { current =>
      println(s"Looking at ${(current + indexOfZerob) % sizeOfArray} => ${resultb.last.mixWith(numbers)((current + indexOfZerob) % sizeOfArray)}")
      resultb.last.mixWith(numbers)((current + indexOfZerob) % sizeOfArray)
    }.sum
    println(s"$resultPart1")*/
    //println(indexesOrdered.mixWith(numbers))
    //println(indexesOrderedStep1.mixWith(numbers))
    /*var workingArray = (0 until sizeOfArray).toArray
    numbers.zipWithIndex.foreach:
      case (number, index) =>
        val destination =
          (workingArray(index) + number) match
            case value if value < 0 => sizeOfArray + value
            case _ => ((workingArray(index) + number) % sizeOfArray)
        val currentValue = workingArray(destination)
        workingArray(index) = destination
        workingArray(destination) = index
        println(workingArray.toList)

    println(workingArray.toList)*/



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

case class IndexesOrdered(values: Seq[Long]):
  val asArray = values.toArray
  val length = asArray.length

  def reverse = IndexesOrdered(values.reverse)

  private def moveInternally(nTh: Int, to: Int): IndexesOrdered =
    val withoutCurrent = values.filterNot(_ == asArray(nTh))
    val newList = (withoutCurrent.take(to) :+ asArray(nTh)) :++ withoutCurrent.drop(to)
    //println(s"in ${asArray.toList} : moving $nTh [${asArray(nTh)}] to $to : $values   => $newList ($withoutCurrent)")
    IndexesOrdered(newList)

  def move2(index: Int, of: Int): IndexesOrdered =
    @tailrec
    def moveRec(current: IndexesOrdered, index: Int, nbOfTimes: Long, mover: (IndexesOrdered, Int) => (IndexesOrdered, Int)): IndexesOrdered =
      nbOfTimes match
        case 0 => current
        case _ =>
          val (newIO, newIndex) = mover.apply(current, index)
          moveRec(newIO, newIndex, nbOfTimes - 1, mover)
    val indexOfValueToMove = asArray.indexOf(index)
    of match
      case value if value > 0 => moveRec(this, indexOfValueToMove, of, IndexesOrdered.moveForward)
      case value if value < 0 => moveRec(this, indexOfValueToMove, -of, IndexesOrdered.moveBackward)
      case _ => this



  def move(index: Int, of: Long): IndexesOrdered =
    of match
      case 0 => this
      case value if value < 0 => this.reverse.move(index, -of).reverse
      case value =>
        val indexOfValueToMove = asArray.indexOf(index)
        val newPosition = indexOfValueToMove + of match
          case value if value > length - 1 => (value % (length-1)).toInt
          case value => value.toInt

        moveInternally(indexOfValueToMove, newPosition)

    /*of match
      case value if value < 0 => this
      case value =>
        val withoutCurrent = values.filterNot(_ == asArray.indexOf(index))
        println(s"here $indexOfValueToMove")
        IndexesOrdered((withoutCurrent.take((indexOfValueToMove + of)%length) :+ asArray.indexOf(index)) :++ withoutCurrent.drop((indexOfValueToMove + of)%length))*/

  def mixWith(data: Seq[Long]): Seq[Long] =
    val dataAsArray = data.toArray
    values.map(value => data(value.toInt))

object IndexesOrdered:
  private def moveForward(on: IndexesOrdered, nTh: Int): (IndexesOrdered, Int) =
    val withoutCurrent = on.values.filterNot(_ == on.asArray(nTh))
    val (newList, newIndex) = nTh match
      case value if value == on.length - 1 => ((withoutCurrent.take(1) :+ on.asArray(nTh)) :++ withoutCurrent.drop(1), 1)
      case value => ((withoutCurrent.take(nTh + 1) :+ on.asArray(nTh)) :++ withoutCurrent.drop(nTh + 1), nTh + 1)
    (IndexesOrdered(newList), newIndex)

  private def moveBackward(on: IndexesOrdered, nTh: Int): (IndexesOrdered, Int) =
    val withoutCurrent = on.values.filterNot(_ == on.asArray(nTh))
    val (newList, newIndex) = nTh match
      case value if value == 0 => ((withoutCurrent.take(on.length - 2) :+ on.asArray(nTh)) :++ withoutCurrent.drop(on.length - 2), on.length - 2)
      case value if value == 1 => ((withoutCurrent.take(on.length - 1) :+ on.asArray(nTh)) :++ withoutCurrent.drop(on.length - 1), on.length - 1)
      case value => ((withoutCurrent.take(nTh - 1) :+ on.asArray(nTh)) :++ withoutCurrent.drop(nTh - 1), nTh - 1)
    (IndexesOrdered(newList), newIndex)