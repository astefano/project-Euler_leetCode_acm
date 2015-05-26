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
  val s = Array.ofDim[Int](n, n)  
  val indices = List.range(0, n)

	 indices foreach {
	   i =>
	     indices foreach {
	       j => 
		 s(i)(j) = MAX
	     }
	 }
    s(0)(0) = mat(0)(0)

List.range(1,n*n).dropWhile {
//List.range(1,n*n) foreach {
 k =>
  var flag = false
	 indices foreach {
	   i2 =>
	     indices foreach {
	       j2 => 
		     if (i2 > 0 || j2 > 0) {
		     val l1 = if (i2 == 0) MAX else s(i2-1)(j2)
		     val l2 = if (i2 == n-1) MAX else s(i2+1)(j2)
		     val l3 = if (j2 == 0) MAX else s(i2)(j2-1)
		     val l4 = if (j2 == n-1) MAX else s(i2)(j2+1)
		     val m = List(l1, l2, l3, l4).min
		       if (s(i2)(j2) == MAX || s(i2)(j2) > m + mat(i2)(j2)) {		     
			 s(i2)(j2) = m + mat(i2)(j2) 
			 flag = true
		       }
		   //if (s(i2)(j2) != MAX && s(i2)(j2) != 0) 
		   //  println("s("+ i2 + ", " + j2 + ", " + k + ") = " + s(i2)(j2)  + " m = " + m + " l1 = " + l1 + " i2 = " + i2)
		 }
	     }
	 }
     flag
}

  println("res = " + s(n-1)(n-1))
  println( "Time (s): " + ( System.currentTimeMillis() - start ) / 1000. )
  }
}
