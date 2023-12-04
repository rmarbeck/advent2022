import scala.io.Source
import scala.math._


@main def hello: Unit =
  println("Launching 5-12")
  val bufferedSource = Source.fromFile("./src/main/resources/test1.txt")
  val data = bufferedSource.getLines().toSeq
  data.foreach(Cargo.digest(_))
  Cargo.displayingStacks
  val result1 = Cargo.stacks1.map(_.head).mkString
  val result2 = Cargo.stacks2.map(_.head).mkString
  println(s"1: $result1")
  println(s"2: $result2")
  Cargo.displayingStacks

object Cargo:
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
    //displayingStacks
  def initialize(rows: String): Unit =
    nbOfStacks = rows.replaceAll(" +", ",").split(',').reverse.head.toInt
    stacks1 = Array.fill(nbOfStacks)(Seq())
    stacks2 = Array.fill(nbOfStacks)(Seq())
    //println(s"initialize $nbOfStacks rows")
    populateStacks
    displayingStacks
    //println(stacks)
    initialized = true
  def populateStacks: Unit =
    println(s"populatingStacks")
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
  def displayingStacks: Unit =
    val range = stacks2.map(_.length).max to 0 by -1
    println(range.map(outsideIndex =>
      (0 to stacks2.length - 1).map(insideIndex =>
        stacks2(insideIndex) match
          case sequence if sequence.length > outsideIndex => s"[${sequence.reverse(outsideIndex)}]"
          case _ => s"   "
        ).mkString(" ")
      ).mkString("\n")
    )
    println((0 to stacks2.length - 1).map(insideIndex => s" $insideIndex  ").mkString)

    //println(stacks.map(_.mkString("-")).mkString(","))
    ()




