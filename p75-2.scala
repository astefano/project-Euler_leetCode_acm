object P75 {

import scalax.io._
import scalax.file.{ FileOps, Path, NotFileException }

  def main(args: Array[String]) {
    val lim = args(0).toInt
    var n = 0
    val start = System.currentTimeMillis
    val file = "/media/ubuntuPart2/docs/topcoder/perimss.txt"   
    val rf: Input = Resource.fromFile(file)
    val lines = rf.lines().filter(_!="")
    val nlines = lines.size
    println("n lines = " + nlines)
    var s = 0L
    var clines = 0
    val perims = lines.toList.map{li => li.trim.split(" ").filter(_!="").map{_.toInt}}.map{x => x(0)}
    val lp = perims.length
    println("lp = " + lp)
    val pa = Array.ofDim[Int](lim/2+1)
    perims foreach {
      p => 
	val k = lim/p 
	List.range(1, k+1) foreach {
	  ki => 
	    pa(p*ki/2) += 1
	}
    }    
    println("res = " + pa.filter(_==1).length)
    println( "Time (s): " + ( System.currentTimeMillis() - start ) / 1000. )
  }
}
