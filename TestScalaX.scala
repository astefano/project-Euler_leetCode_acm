object TestScalaX{

  import scalax.io._
  import scalax.file.{ FileOps, Path, NotFileException }

def main(args: Array[String]) {
  val file = "in.txt"
  val rf: Input = Resource.fromFile(file)
  val lines = rf.lines().filter(_!="")
  println("lines = " + lines.toList)
  }
} 
