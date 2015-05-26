object P81 {

  import scalax.io._
  import scalax.file.{ FileOps, Path, NotFileException }

  def main(args: Array[String]) {
  val start = System.currentTimeMillis
  val file = "/media/ubuntuPart2/docs/topcoder/matrix.txt"
  val rf: Input = Resource.fromFile(file)

  val lines = rf.lines().filter(_!="")
  val n = lines.size
  println("n lines = " + n)
  var i = 0
  val s = Array.ofDim[Long](n, n)
  lines foreach {
      l => 
	val row = l.trim.split(",").toList.map{_.toLong}	
	if (i == 0) {
	  s(0)(0) = row(0)
	  List.range(1,n) foreach {
	    j => 
	      s(0)(j) = s(0)(j-1) + row(j)
	  }
	}
	else {
	  s(i)(0) = s(i-1)(0) + row(0)   	 
	  List.range(1, n) foreach {
	    j => 
	      s(i)(j) = math.min(s(i-1)(j), s(i)(j-1)) + row(j)
	  }
	}
	i += 1
  }
  println("res = " + s.toList.foldLeft("")(_+"\n"+_.toList))
  println( "Time (s): " + ( System.currentTimeMillis() - start ) / 1000. )
  }
}
