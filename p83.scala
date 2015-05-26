object P83 {

  import scalax.io._
  import scalax.file.{ FileOps, Path, NotFileException }

  def main(args: Array[String]) {
  val start = System.currentTimeMillis
  val file = "/media/ubuntuPart2/docs/topcoder/matrixP83.txt"
  val rf: Input = Resource.fromFile(file)
  val lines = rf.lines().filter(_!="")
  val n = lines.size
  println("n lines = " + n)
  var i = 0
  val mat = lines.toArray.map(x => x.split(",").map(_.toInt))    
  val MAX = scala.Int.MaxValue
  val s = Array.ofDim[Int](n, n, n, n)  
  val indices = List.range(0, n)
  indices foreach {
    i =>
     indices foreach {
       j => 
	 s(i)(j)(i)(j) = mat(i)(j)
     }
  }

List.range(1,n*n).dropWhile {
//List.range(1,n*n) foreach {
 k =>
  var flag = false
  indices foreach {
    i1 =>
     indices foreach {
       j1 => 
	 indices foreach {
	   i2 =>
	     indices foreach {
	       j2 => 
		     if (i2 > i1 || j2 > j1) {
		     val l1 = if (i2 == 0) MAX else s(i1)(j1)(i2-1)(j2)
		     val l2 = if (i2 == n-1) MAX else s(i1)(j1)(i2+1)(j2)
		     val l3 = if (j2 == 0) MAX else s(i1)(j1)(i2)(j2-1)
		     val l4 = if (j2 == n-1) MAX else s(i1)(j1)(i2)(j2+1)
		     val l1n = if (l1 == 0) MAX else l1
		     val l2n = if (l2 == 0) MAX else l2
		     val l3n = if (l3 == 0) MAX else l3
		     val l4n = if (l4 == 0) MAX else l4
		     val m = List(l1n, l2n, l3n, l4n).min
		     if ( m != MAX) {
		       if (s(i1)(j1)(i2)(j2) > m + mat(i2)(j2) || s(i1)(j1)(i2)(j2) == 0) {		     
			 s(i1)(j1)(i2)(j2) = m + mat(i2)(j2) 
			 flag = true
		       }
		     }
		     else {
		       s(i1)(j1)(i2)(j2) = m
		     } 
		   }
		   //if (s(i1)(j1)(i2)(j2) != MAX && s(i1)(j1)(i2)(j2) != 0) 
		     //println("s("+i1 + ", " + j1 + ", " + i2 + ", " + j2 + ", " + k + ") = " + s(i1)(j1)(i2)(j2) )
		 }
	     }
	 }
     }
     flag
    }

  println("res = " + s(0)(0)(n-1)(n-1))
  println( "Time (s): " + ( System.currentTimeMillis() - start ) / 1000. )
  }
}
