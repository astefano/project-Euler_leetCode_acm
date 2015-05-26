/* https://code.google.com/codejam/contest/32003/dashboard#s=p1
*/
object turnL {

  import scalax.io._
  import scalax.file.{ FileOps, Path, NotFileException }

  def main(args: Array[String]) {
    val start = System.currentTimeMillis

    //val file = "testSpell.in"   
    //val file = "C-small-practice.in"   
    val file = "C-large-practice.in"   
    val rf: Input = Resource.fromFile(file)

    //val fileOut = "C-small-practice.out"   
    val fileOut = "C-large-practice.out"   
    val rfo: Output = Resource.fromFile(fileOut)

    val lines = rf.lines().filter(_!="")
    val nlines = lines.size
    var out = ""

    val ncases = lines(0).toInt
    (0 to ncases-1) foreach {
      i => 
	out += "Case #"+(i+1)+": " + ... + "\n"
    }
    //println(out)
    rfo.write(out)
    println( "Time (s): " + ( System.currentTimeMillis() - start ) / 1000. )
  }

}
