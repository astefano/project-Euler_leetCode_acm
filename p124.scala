object P124 {


  import scalax.io._
  import scalax.file.{ FileOps, Path, NotFileException }

  def main(args: Array[String]) {
    val start = System.currentTimeMillis
    
    val file = "/media/ubuntuPart2/docs/topcoder/outP124.txt"
    val rf: Input = Resource.fromFile(file)

    val lines = rf.lines().filter(_!="")
    val nlines = lines.size
    println("n lines = " + nlines)
    val l = lines.toList.zipWithIndex.map{x => (x._1.toInt, x._2.toInt+1)}
    val ls = l.sortBy(_._1)
/*    var clines = 0
    var s = BigInt(0)
    lines foreach {
      li => 
	clines = clines + 1
        s += BigInt(li)
    }
*/   
    println(l.take(10))
    println(ls.take(50))
    println("ls(10000) = " + ls(10000)) 
    println("ls(10000) = " + ls(10000-1)) 
    println( "Time (s): " + ( System.currentTimeMillis() - start ) / 1000. )
  }
} 


