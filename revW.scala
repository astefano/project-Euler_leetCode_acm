object revW {

 import scalax.io._
  import scalax.file.{ FileOps, Path, NotFileException }

  def main(args: Array[String]) {
    val start = System.currentTimeMillis

    //val file = "B-small-practice.in"   
    val file = "B-large-practice.in"   
    val rf: Input = Resource.fromFile(file)

    //val fileOut = "B-small-practice.out"   
    val fileOut = "B-large-practice.out"   
    val rfo: Output = Resource.fromFile(fileOut)

    val lines = rf.lines().filter(_!="")
    val nlines = lines.size
    var out = ""

    val ncases = lines(0).toInt
    (0 to ncases-1) foreach {
      i => 
	out += "Case #"+(i+1)+": " + lines(i+1).split(" ").toList.reverse.reduceLeft(_+" "+_) + "\n"
    }
    rfo.write(out)
    println( "Time (s): " + ( System.currentTimeMillis() - start ) / 1000. )
  }
}
