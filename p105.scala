object P105{

  def valid(s: Set[Int]): Boolean = {    
    val all = (s.subsets.toSet - Set[Int]()).toList
    var stop = false
    val rem = all.dropWhile {
      b => 
	val r0 = all.filter(x => ((x & b) == Set()))
	//println("r0 = " + r0)
	val r1 = r0.dropWhile {
	  c => 
	  val sat = !(b.sum == c.sum || (b.size > c.size && b.sum < c.sum))
	  //if (!sat) println("counterex b = " + b + " c = " + c)
	  (sat)
	}
	//if(r1 != List()) println("b = " + b + " r1 = " + r1)
      (r1 == List())
    } 
    //println("rem = " + rem)
    rem == List()
  }  

  import scalax.io._
  import scalax.file.{ FileOps, Path, NotFileException }

  def main(args: Array[String]) {
    val start = System.currentTimeMillis

    val file = "/media/ubuntuPart2/docs/topcoder/p105.txt"
    val rf: Input = Resource.fromFile(file)

    val lines = rf.lines().filter(_!="")
    val nlines = lines.size
    println("n lines = " + nlines)
    var ss = 0
    lines foreach {
      li => 
      	 val s = li.split(",").toList.map{_.trim.toInt}.toSet
	 if (valid(s)) {
	   println("valid: " + s.toList.sorted)
	   ss += s.sum
	 }
    }

    println("res = " + ss)

    println( "Time (s): " + ( System.currentTimeMillis() - start ) / 1000. )
  }

}
