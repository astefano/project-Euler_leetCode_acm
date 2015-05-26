object P113 {

  def isInc(n : Int) : Boolean = {
    val ns = n.toString
    val l = ns.length
    val indices = List.range(0, l-1)
    val r = indices.dropWhile(i => ns(i) <= ns(i+1))
    (r==List())
  }

  def isDec(n : Int) : Boolean = {
    val ns = n.toString
    val l = ns.length
    val indices = List.range(0, l-1)
    val r = indices.dropWhile(i => ns(i) >= ns(i+1))
    (r==List())
  }

  def main(args: Array[String]) {

    val start = System.currentTimeMillis

    //the max nb of digits
    val nd = args(0).toInt
    /* notB(i)(j) a pair (nbi, nbd) with:
     * nbi the number of numbers of i digits in increasing order ending in digit j
     * nbd the number of numbers of i digits in decreasing order ending in digit j
     */ 
    val notB = Array.fill(nd+1, 10)((0L,0L))
    //init: the nb of numbers of 1 digit ending in i is 1
    List.range(0,10) foreach {
      j => 
	notB(1)(j) = (1L, 1L)
	notB(2)(j) = (j, 10L - j)	  
    }
    //correct nb of numbers of 2 digits in dec order ending in 0 is 9 (10, .., 90)
    notB(2)(0) = (0L, 9L)

    List.range(3,nd+1) foreach {
      i => 
	    List.range(1,10) foreach {
	      j => 
		val nbi = List.range(1,j+1).foldLeft(0L)( (r, c) => (r + notB(i-1)(c)._1) )
		val nbd = List.range(j, 10).foldLeft(0L)( (r, c) => (r + notB(i-1)(c)._2) )
		notB(i)(j) = (nbi, nbd) 
	    }
	    //dec numbers ending in 0
	    notB(i)(0) = (0L, List.range(0, 10).foldLeft(0L)((r,c)=>(r + notB(i-1)(c)._2)))
    }

    //delete the 10 1-digit nbs (are these inc? are they dec??)
    val temp = notB.toList.drop(2).map(_.toList)
    val s = temp.flatten.foldLeft((0L, 0L))( (r, c) => ( (c._1 + r._1, c._2 + r._2) ) ) 
    //println("notBInc = " + temp.foldLeft("")(_+"\n"+_))
    println((s._1 + s._2 + 10 - 9*(nd-1)))
    println( "Time (s): " + ( System.currentTimeMillis() - start ) / 1000. )
  }
}

