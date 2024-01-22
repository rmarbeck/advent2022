import scala.io.Source
import com.typesafe.scalalogging.Logger

import scala.annotation.tailrec
import scala.collection.mutable

val loggerAOC = Logger("aoc")
val loggerAOCPart1 = Logger("aoc.part1")
val loggerAOCPart2 = Logger("aoc.part2")

@main def hello: Unit =
  loggerAOC.trace("Root trace activated")
  loggerAOC.debug("Root debug activated")
  println("Launching Day21")
  List[() => (String, String)]( () => Solver.solveTest, () => Solver.solve).foreach: f =>
    val (score1, score2) = f.apply()
    println(s"1 : ${score1}")
    println(s"2 : ${score2}")
    println(s"----------------")
  println("Done")

object Solver:
  def runOn(inputLines: Seq[String]): (String, String) =

    val (root, specialMonkey) = ("root", "humn")

    val monkeys = inputLines.map:
      case s"$name: $first $operator $second" => OperationMonkey(name, MathOperation(first, second, Operator.from(operator)))
      case s"$name: $value" => SpecificNumberMonkey(name, value.toInt)

    val jungle = Jungle(monkeys)
    given Jungle = jungle

    val (firstMonkey, secondMonkey) = jungle.getMonkey(root) match
      case OperationMonkey(_, MathOperation(first, second, _)) => (first, second)
      case _ => throw Exception("Not supported")

    @tailrec
    def dichotomy(value: Long, step: Long, firstMonkey: Monkey, target: Long): Long =
      val firstValue = firstMonkey.yell(using jungle.update(specialMonkey, value))
      val firstValuePlusStep = firstMonkey.yell(using jungle.update(specialMonkey, value+step))
      step match
        case 0 => println("approximate"); value
        case _ =>
          firstValue - target match
            case 0 => value
            case diff if diff < 0 => firstValuePlusStep - target match
              case 0 => value+step
              case diffWithStep if diffWithStep < 0 => dichotomy(value + step, step * 2, firstMonkey, target)
              case _ => dichotomy(value, step / 2, firstMonkey, target)
            case _ => firstValuePlusStep - target match
              case 0 => value+step
              case diffWithStep if diffWithStep > 0 => dichotomy(value + step, step * 2, firstMonkey, target)
              case _ => dichotomy(value-step, step / 2, firstMonkey, target)


    def lowestSolution(value: Long, step: Long, firstMonkey: Monkey, target: Long): Long =
      val rawResult = dichotomy(value, step, firstMonkey, target)
      def lazyList(index: Long): LazyList[Long] = (rawResult - index) #:: lazyList(index + 1l)
      lazyList(1l).find:
        case currentValue => firstMonkey.yell(using jungle.update(specialMonkey, currentValue)) != target
      .map(_ + 1).getOrElse(rawResult)

    //second monkey does not depend on humn value
    val target = jungle.getMonkey(secondMonkey).yell
    val (startValue, unsignedStep) = (0, 100000)
    val valueAtStart = jungle.getMonkey(firstMonkey).yell(using jungle.update(specialMonkey, startValue))

    //should test monotony to be sure
    val stepToUse = valueAtStart < target match
        case true => unsignedStep
        case _ => -unsignedStep

    val resultPart2 = lowestSolution(startValue, stepToUse, jungle.getMonkey(firstMonkey), target)

    val result1 = s"${jungle.getMonkey(root).yell}"
    val result2 = s"$resultPart2"

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

enum Operator:
  case Plus, Minus , Multiply, Divide

  override def toString =
    this match
      case Plus => "+"
      case Minus => "-"
      case Multiply => "*"
      case Divide => "/"

object Operator:
  def from(op: String) =
    op match
      case "+" => Plus
      case "-" => Minus
      case "*" => Multiply
      case "/" => Divide
      case _ => throw Exception("Not Supported")
export Operator._

case class Jungle(monkeys: Seq[Monkey]):
  private val monkeysMap = mutable.HashMap[String, Monkey]()
  monkeys.foreach:
    case monkey: Monkey => monkeysMap.put(monkey.name, monkey)
  def getMonkey(name: String): Monkey = monkeysMap.get(name).get
  def update(name: String, value: Long): Jungle = Jungle(monkeys.map:
    case SpecificNumberMonkey(currentName, _) if currentName == name => SpecificNumberMonkey(currentName, value)
    case monkey => monkey)

case class MathOperation(first: String, second: String, operator: Operator)

sealed trait Monkey(val name: String):
  def yell(using Jungle): Long
  def describe(using Jungle): String

case class SpecificNumberMonkey(nameOf: String, value: Long) extends Monkey(nameOf):
  override def yell(using Jungle): Long = value
  override def describe(using Jungle): String = s"$value"

case class OperationMonkey(nameOf: String, operation: MathOperation) extends Monkey(nameOf):
  override def yell(using jungle: Jungle): Long =
    val firstValue = jungle.getMonkey(operation.first).yell
    val secondValue = jungle.getMonkey(operation.second).yell
    operation.operator match
      case Plus => firstValue + secondValue
      case Minus => firstValue - secondValue
      case Multiply => firstValue * secondValue
      case Divide => firstValue / secondValue
  override def describe(using jungle: Jungle): String = s"(${jungle.getMonkey(operation.first).describe} ${operation.operator} ${jungle.getMonkey(operation.second).describe})"