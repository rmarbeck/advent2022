import scala.collection.mutable
import scala.io.Source
import scala.reflect.{ClassTag, TypeTest}

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
    val bufferedSource = Source.fromResource(fileName)
    val lines = bufferedSource.getLines().toSeq

    val commands = lines.map:
      case s"$$ cd .." => ChangeDirBackward()
      case s"$$ cd $name" => ChangeDirForward(name)
      case s"$$ ls" => ListDir()
      case s"dir $name" => DirectoryName(name)
      case s"$size $name" => FileNameAndSize(name, size.toLong)

    val part1Limit = 100000

    val totalSpace = 70000000l
    val minSpaceNeeded = 30000000l
    val fileSystem = FileSystem(commands)
    val spaceToFreeUp = fileSystem.size + minSpaceNeeded - totalSpace

    val result1 = s"${fileSystem.sizeAtMost(part1Limit)}"
    val result2 = s"${fileSystem.smallestAbove(spaceToFreeUp)}"

    (s"${result1}", s"${result2}")
end Solver

sealed trait CommandOrResult

sealed trait Command extends CommandOrResult

case class ChangeDirForward(name: String) extends Command
class ChangeDirBackward extends Command
class ListDir extends Command

sealed trait Result extends CommandOrResult

class DirectoryName(val name: String) extends Result

object DirectoryName:
  def unapply(dir: DirectoryName)= Some(dir.name)

class FileNameAndSize(val name: String, val size: Long) extends Result

object FileNameAndSize:
  def unapply(file: FileNameAndSize) = (file.name, file.size)

sealed trait FileOrDirectory(val name: String):
  def size: Long
  def display(indentLevel: Int): String

class File(name: String, val size: Long) extends FileOrDirectory(name):
  override def display(indentLevel: Int) =
    val indent = " " * indentLevel
    s"$indentLevel $indent - $name (file, size=$size)"

  override def toString: String = s"$name - $size"

class Directory(name: String, parent: Option[Directory]) extends FileOrDirectory(name):
  override def display(indentLevel: Int) =
    val indent = " "*indentLevel
    s"""$indentLevel $indent - $name (dir)
       |${children.values.map(_.display(indentLevel+1)).mkString("\n")}""".stripMargin

  override def toString: String = s"dir $name (${parent.size} children)"
  override def size: Long = children.values.map(_.size).fold(0l)(_+_)

  def sizeAtMost(sizeLimit: Int) = size match
    case value if value < sizeLimit => value
    case _ => 0

  def getDirectories: Seq[Directory] = children.values.flatMap:
    case dir: Directory => dir +: dir.getDirectories
    case _ => Nil
  .toSeq

  def getParent = parent

  val children = mutable.LinkedHashMap[String, FileOrDirectory]()
  def add(child: FileOrDirectory): Unit = children += child.name -> child

  def getChildDirectory(name: String): Directory = name match
    case value if value == "/" => this
    case value => children.get(value) match
      case Some(directory:  Directory) => directory
      case _ => throw Exception(s"Not found dir for name ${value} ${children} in $this")


class FileSystem(input: Seq[CommandOrResult]):
  def display = root.display(0)
  def size = root.size
  def sizeAtMost(sizeLimit: Int) = root.getDirectories.filter(_.sizeAtMost(sizeLimit) != 0).map(_.size).sum
  def smallestAbove(sizeLimit: Long) = root.getDirectories.map(_.size).sorted.filter(_ >= sizeLimit).head
  val root = Directory("/", None)
  var currentDir = root
  for currentLine <- input do
    currentLine match
      case ChangeDirForward(name) => currentDir = currentDir.getChildDirectory(name)
      case cd: ChangeDirBackward => currentDir.getParent match
        case Some(directory) => currentDir = directory
        case _ => ()
      case ls: ListDir => ()
      case DirectoryName(name) => currentDir.add(Directory(name, Some(currentDir)))
      case FileNameAndSize(name, size) => currentDir.add(File(name, size))

