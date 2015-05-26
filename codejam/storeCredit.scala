object storeCredit {

  import scalax.io._
  import scalax.file.{ FileOps, Path, NotFileException }

  def main(args: Array[String]) {
    val start = System.currentTimeMillis

    //val file = "A-small-practice.in"   
    val file = "A-large-practice.in"   
    val rf: Input = Resource.fromFile(file)

    //val fileOut = "A-small-practice.out"   
    val fileOut = "A-large-practice.out"   
    val rfo: Output = Resource.fromFile(fileOut)

    val lines = rf.lines().filter(_!="")
    val nlines = lines.size
    var out = ""
    val ncases = lines(0).toInt
    (0 to ncases-1) foreach {
      i => 
	out += "Case #"+(i+1)+": "
	val cSum = lines(3*i+1).toInt
	//println("s = " + cSum)
	val clen = lines(3*i+2).toInt
	//println("len = " + clen)
	val cList = lines(3*i+3).split(" ").toList.map(_.toInt)      
	//println("l = " + cList)
	//val oppositeL = cList.map(x => cSum - x)
	var stop = false
	(0 to clen-1).dropWhile {
	  k => 
	   (k+1 to clen-1).dropWhile {
	     j => 
	        if (cList(k) + cList(j) == cSum) {
		  out += (k+1) + " " + (j+1) + "\n"
		  stop = true
		}
	       (cList(k) + cList(j) != cSum)
	   }
	  (!stop)
	}
	//println(out)
    }
    //println(out)
    rfo.write(out)
    println( "Time (s): " + ( System.currentTimeMillis() - start ) / 1000. )
  }
}

