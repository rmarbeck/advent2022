import scala.io.Source
import com.typesafe.scalalogging.Logger

import scala.collection.mutable

val loggerAOC = Logger("aoc")
val loggerAOCPart1 = Logger("aoc.part1")
val loggerAOCPart2 = Logger("aoc.part2")

@main def hello: Unit =
  loggerAOC.trace("Root trace activated")
  loggerAOC.debug("Root debug activated")
  println("Launching Day11")
  List[() => (String, String)]( () => Solver.solveTest, () => Solver.solve).foreach: f =>
    val (score1, score2) = f.apply()
    println(s"1 : ${score1}")
    println(s"2 : ${score2}")
    println(s"----------------")
  println("Done")

object Solver:
  def runOn(inputLines: Seq[String]): (String, String) =
    def monkeys = inputLines.filterNot(_.isEmpty).grouped(6).map(_.map(_.trim).mkString(";")).map:
      case s"Monkey $monkeyId:;Starting items: $items;Operation: new = $operation;Test: divisible by $divisor;If true: throw to monkey $monkeyTrue;If false: throw to monkey $monkeyFalse" => Monkey(monkeyId.toInt, items.split(", ").map(_.toLong).toList, operation, divisor.toLong, monkeyTrue.toInt, monkeyFalse.toInt)
    .toList

    val junglePart1  = Jungle()
    monkeys.foreach:
      junglePart1.addMonkey(_)

    junglePart1.runNCycleWithRelief(20, 3l)

    val junglePart2 = Jungle()
    monkeys.foreach:
      junglePart2.addMonkey(_)
    junglePart2.runNCycleWithRelief(10_000, 1l)

    val result1 = s"${junglePart1.getTwoMostActive.map(_.numberOfInspections).product}"
    val result2 = s"${junglePart2.getTwoMostActive.map(_.numberOfInspections.toLong).product}"

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

class Jungle:
  lazy val lcm = monkeys.values.toSeq.map(_.tester).product
  val monkeys = mutable.HashMap[Int, Monkey]()
  def findMonkey(id: Int): Monkey =
    monkeys.get(id).get
  def addMonkey(monkey: Monkey): Jungle =
    monkeys += monkey.id -> monkey
    this

  def runNCycleWithRelief(numberOfCycles: Int, relief: Long): Unit =
    lcm
    for cycle <- 1 to numberOfCycles do
      monkeys.keys.toSeq.sorted.foreach:
          monkeys.get(_).get.workOnCycle(cycle, relief)(this)

  def getTwoMostActive: List[Monkey] =
    monkeys.values.toList.sortBy(_.numberOfInspections).takeRight(2)

  override def toString: String = s"Jungle is populated by ${monkeys.size} monkeys : ${monkeys.values}"

class Monkey(val id: Int, var items: List[Long], val operation: String, val tester: Long, val ifTrue: Int, val ifFalse: Int):
  var numberOfInspections = 0
  private lazy val compiledOperation: (Long) => Long = operation match
    case s"old + old" => item => item + item
    case s"old * old" => item => item * item
    case s"old + $value" => item => item + value.toInt
    case s"old * $value" => item => item * value.toInt
    case _ => throw Exception("Unrecognized operation")

  def receiveItem(i: Long): Unit =
    items = items :+ i
  private def applyOperation(itemValue: Long): Long =
    compiledOperation(itemValue)
  override def toString: String = s"I am Monkey n°$id and I have ${items.length} items ${items}, having inspected $numberOfInspections items"
  def workOnCycle(cycleNumber: Int, relief: Long)(implicit jungle: Jungle): Unit =
    def manageItem(item: Long): Unit =
      def sendTo(newItem: Long, monkey: Int): Unit =
        jungle.findMonkey(monkey).receiveItem(newItem)
        items = items.tail
      numberOfInspections += 1
      val newItem: Long = (applyOperation(item) / relief) % jungle.lcm
      newItem % tester match
        case 0 => sendTo(newItem, ifTrue)
        case _ => sendTo(newItem, ifFalse)
    if cycleNumber == 1 then
      numberOfInspections = 0
    items.foreach:
      manageItem(_)