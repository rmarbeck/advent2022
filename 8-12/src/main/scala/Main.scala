  import scala.collection.mutable
  import scala.io.Source
  import scala.reflect.{ClassTag, TypeTest}

// Right :-/ result is

@main def hello: Unit =
  println("Launching 7-12")
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
    val bufferedSource = Source.fromFile("./src/main/resources/" + fileName)
    val lines = bufferedSource.getLines().toSeq

    val container = Container(lines)

    //println(container)

    val resultPart1 = (for row <- 0 until container.height
        col <- 0 until container.width yield
        container.isVisible(row, col)
      ).count(_ == true)

    val resultPart2 = (for row <- 1 until container.height - 1
                      col <- 1 until container.width - 1 yield
      container.surrounding(row, col)
      ).max

    val result1 = s"$resultPart1"
    val result2 = s"$resultPart2"

    (s"${result1}", s"${result2}")
end Solver

class Container(input: Seq[String]):
  val width = input(0).length
  val height: Int = input.length
  private val data: Array[Array[Int]] = input.toArray.map(_.toArray.map(_.asDigit))

  def getAllSubLists(row: Int, col: Int): List[Seq[Int]] =
    val toUp = (row - 1 to 0 by -1).map(data(_)(col))
    val tilDown = (row + 1 until height).map(data(_)(col))
    val toLeft = (col - 1 to 0 by -1).map(data(row)(_))
    val toRight = (col + 1 until width).map(data(row)(_))

    List(toUp, tilDown, toLeft, toRight)

  def surrounding(row: Int, col: Int): Int =
    getAllSubLists(row, col).map: listOfInt =>
      listOfInt.span(_ < data(row)(col))
    .map((part1, part2) => part1.length + part2.indexWhere(_ >= data(row)(col)) + 1).product

  private def minOfMax(row: Int, col: Int): Int =
    getAllSubLists(row, col).map(_.sorted.maxOption.getOrElse(-1)).sorted.head

  def isVisible(row: Int, col: Int): Boolean =
    data(row)(col) > minOfMax(row, col)

  def asString(data: Array[Array[Char]]) =
    data.map(_.mkString(" ")).mkString("\n")

  override def toString =
    asString(data.map(_.map(_.toString.head)))
