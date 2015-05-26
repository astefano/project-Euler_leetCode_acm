object allBase {

 import scalax.io._
  import scalax.file.{ FileOps, Path, NotFileException }

  def findMin(s: String) = ...


  def main(args: Array[String]) {
    val start = System.currentTimeMillis

    //val file = "small.in"   
    val file = "large.in"   
    val rf: Input = Resource.fromFile(file)

    //val fileOut = "small.out"   
    val fileOut = "large.out"   
    val rfo: Output = Resource.fromFile(fileOut)

    val lines = rf.lines().filter(_!="")
    val nlines = lines.size
    var out = ""

    val ncases = lines(0).toInt
    (0 to ncases-1) foreach {
      i => 
	out += "Case #"+(i+1)+": " + findMin(lines(i+1)) + "\n"
    }
    rfo.write(out)
    println( "Time (s): " + ( System.currentTimeMillis() - start ) / 1000. )
  }
}
