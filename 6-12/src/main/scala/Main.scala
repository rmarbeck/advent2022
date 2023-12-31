  import scala.io.Source

// Right :-/ result is

@main def hello: Unit =
  println("Launching 6-12")
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
    //lines.zipWithIndex.foreach((line, index) => println(s"$index => ${fromLine(line)}"))
    val result1 = lines.map(fromLine(_, 4)).mkString("-")
    val result2 = lines.map(fromLine(_, 14)).mkString("-")
    (s"${result1}", s"${result2}")
end Solver

def fromLine(line: String, size: Int): Int =
  def manage(char: Char, fromCurrentList: (List[Char], Int)): (List[Char], Int) =
    val (currentList, index) = fromCurrentList
    currentList.length match
      case `size` => fromCurrentList
      case _ =>
        if (currentList.contains(char))
          (currentList.drop(currentList.indexOf(char)+1) :+ char, index + 1)
        else
          (currentList :+ char, index + 1)

  line.foldLeft((List[Char](), 0))((acc, current) => manage(current, acc))._2