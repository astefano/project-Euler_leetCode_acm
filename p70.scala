object P54 {

  import scalax.io._
  import scalax.file.{ FileOps, Path, NotFileException }

  def isPerm(s1: String, s2: String) : Boolean = {
    if (s1 == "" && s2 == "") true
    else if (s1.length != s2.length) false
    else {
      val i = s2.indexOf(s1.head)
      if (i < 0) false
      else isPerm(s1.tail, s2.take(i)+s2.drop(i+1))
    }
  }

  def load = {
    val file = "/media/ubuntuPart2/docs/topcoder/outP70F.txt"
    val rf: Input = Resource.fromFile(file)
    val lines = rf.lines().filter(_!="")
    val nlines = lines.size
    println("n lines = " + nlines)
    var n = 2
    var min = 1.001
    var lastn = 2
    lines foreach {
      l => 
	val ll = l.trim.split(" ").filter(_!="").toList
	val phi = ll(0)
	val nmin = 1.*n/phi.toInt
	if (isPerm(phi, n.toString) && min > nmin) {
	  min = nmin
	  lastn = n
	 //if (n < 100) 
	   println(n + " " + phi)	  
	}
	n += 1
    }
    (lastn, min)
  }

  def main(args: Array[String]) {
    println(load)
  }
}

