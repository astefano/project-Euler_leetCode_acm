object P80 {

  import scalax.io._
  import scalax.file.{ FileOps, Path, NotFileException }

  def main(args: Array[String] {
    val start = System.currentTimeMillis
    val file = "/media/ubuntuPart2/docs/topcoder/outP80.txt"   
    val rf: Input = Resource.fromFile(file)
    val lines = rf.lines().filter(_!="")
    val nlines = lines.size
    println("n lines = " + nlines)
    val repr = lines.toList.map(li => li.trim.split(" ").filter(_!="")).map(x => x.mkString)
    repr.map(x => x(0)-'0' + x.drop(2).dropRight(2).foldLeft(0)(_+_-'0')).sum
  }
}
