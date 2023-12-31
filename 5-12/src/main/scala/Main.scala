import scala.io.Source
import scala.math._


@main def hello: Unit =
  println("Launching 5-12")
  List[() => (String, String)]( () => Solver.solveTest, () => Solver.solve).foreach: f =>
    val (score1, score2) = f.apply()
    println(s"1 : ${score1}")
    println(s"2 : ${score2}")
    println(s"----------------")
  println("Done")

object Solver:
  def solveTest: (String, String) =
    solver("test.txt")
  def solve: (String, String) =
    solver("data.txt")
  private def solver(fileName: String): (String, String) =
    val bufferedSource = Source.fromResource(fileName)
    val data = bufferedSource.getLines().toSeq
    val cargo = Cargo()
    data.foreach(cargo.digest(_))
    //Cargo.displayingStacks
    val result1 = cargo.stacks1.map(_.head).mkString
    val result2 = cargo.stacks2.map(_.head).mkString

    (s"${result1}", s"${result2}")
end Solver


class Cargo:
  var initialized = false
  var nbOfStacks = 0
  var stacks1: Array[Seq[Char]] = Array()
  var stacks2: Array[Seq[Char]] = Array()
  var toFillWith: Seq[String] = Seq()
  def digest(line: String): Unit =
    line match
      case s"move $num from $source to $dest" => move(num.toInt, source.toInt-1, dest.toInt-1)
      case s" 1$rows" => initialize(rows)
      case lineContent if lineContent.contains('[') => saveLine(lineContent)
      case _ => ()
  def move(num: Int, source: Int, dest: Int): Unit =
    //println(s"moving $num from $source to $dest")
    stacks1(dest) = stacks1(source).take(num).reverse ++ stacks1(dest)
    stacks1(source) = stacks1(source).splitAt(num)._2
    stacks2(dest) = stacks2(source).take(num) ++ stacks2(dest)
    stacks2(source) = stacks2(source).splitAt(num)._2
    //displayInplace
    //displayingStacks
  def initialize(rows: String): Unit =
    nbOfStacks = rows.replaceAll(" +", ",").split(',').reverse.head.toInt
    stacks1 = Array.fill(nbOfStacks)(Seq())
    stacks2 = Array.fill(nbOfStacks)(Seq())
    //println(s"initialize $nbOfStacks rows")
    populateStacks
    //displayingStacks
    //println(stacks)
    initialized = true
  def populateStacks: Unit =
    //println(s"populatingStacks")
    toFillWith.foreach(stackLine(_))
  def stackLine(lineContent: String): Unit =
    //println(s"stacking line $lineContent")
    lineContent.zipWithIndex.filter((char, index) => char.isLetter).foreach((char, index) => addFromPositionInLine(char, index))
  def addFromPositionInLine(character: Char, indexInLine: Int): Unit =
    val indexInSeq = (((indexInLine+1) + 2) / 4) - 1
    stacks1(indexInSeq) = stacks1(indexInSeq) :+ character
    stacks2(indexInSeq) = stacks2(indexInSeq) :+ character
  def saveLine(lineContent: String): Unit =
    //println(s"saving line $lineContent")
    toFillWith = toFillWith :+ lineContent
  def displayInplace: Unit =
    displayingStacks
    Thread.sleep(10)
    print("\u001b[2J")

  def displayingStacks: Unit =
    val upperLimit = stacks2.map(_.length).max
    val maxNumberOfElementsInStacksTo0 = upperLimit to 0 by -1
    println:
      maxNumberOfElementsInStacksTo0.map: level =>
        stacks2.map:
            case sequence if sequence.length > level => s"[${sequence.reverse(level)}]"
            case _ => s"   "
        .mkString(" ")
      .mkString("\n")

    println((0 to stacks2.length - 1).map(insideIndex => s" $insideIndex  ").mkString)
    println(s"->$upperLimit<-")

    //println(stacks.map(_.mkString("-")).mkString(","))
    ()




