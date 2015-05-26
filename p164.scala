object P164 {

  def main(args: Array[String]) {
    val lim = args(0).toInt

    var m = Array.fill(lim+1, 10, 10)(0L)
    for(i <- (1 to 9); j <- (0 to 9 - i)) m(2)(i)(j) = 1 

    var oldm = Array.fill(10, 10)(0L)
    var newm = Array.fill(10, 10)(0L)
    for(i <- (1 to 9); j <- (0 to 9 - i)) oldm(i)(j) = 1     
    (3 to lim) foreach {
      n => 
	var s = 0L
	(0 to 9) foreach {
	 j => 
	    s = 0 
	    (0 to 9 - j) foreach {
	      i =>
		s += oldm(i)(j)
		(0 to 9 - i - j) foreach {
		  k =>       
		    newm(j)(k) = s
		    m(n)(j)(k) += m(n-1)(i)(j)
		}
	    }
	}
//	for(i <- (0 to 9); j <- (0 to 9 - i); k <- (0 to 9 - i - j)) newm(j)(k) += oldm(i)(j) 
      println(n + ": old = " + oldm.map{_.toList}.toList.flatten.sum)
      for(i <- (0 to 9); j <- (0 to 9-i)) oldm(i)(j) = newm(i)(j)
//      oldm = newm
      println("newm = " + newm.map{_.toList}.toList)
    }
    
    val res = oldm.map{_.toList}.toList.flatten.sum
    println("res = " + res)

/*

    (3 to lim) foreach {
      n => 
	(0 to 9) foreach {
	  i => 
	    (0 to 9 - i) foreach {
	      j =>
		(0 to 9 - i - j) foreach {
		  k => 
		    m(n)(j)(k) += m(n-1)(i)(j)
		}
	    }
	}
    }
    */ 
    val res1 = m(lim).map{_.toList}.toList.flatten.sum
    println("res1 = " + res1)
    
  }
 
  }

