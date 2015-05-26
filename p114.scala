object P114 {

  /* 
   */
  def ways(k: Int, n: Long) : Long = {
    if (n == 0) 1L else if (k == 1) 1L+n else List.range(0L,n+1).reduceLeft((r,c) => r + ways(k-1,n-c))
  }

/*
  def ways3(k: Int, n: Int) : Long = {
    if (n < 4) 0L else if (k == 1) 1L else if (k == 2) n-2L else List.range(3,n-4).foldLeft(0L)((r,c) => r + ways3(k-1,n-c))
  }
*/

val cache = scala.collection.mutable.Map[(Int, Int), Long]((1, 4) -> 1L)

var ncalls = 0 

/* k == 1 is the ways in which rb = n, r >= 3
 */ 
def ways3(k: Int, n: Int) : Long = {
 // ncalls += 1
  if (n < 4) 0L else if (k == 1) n-3L else List.range(4,n-3).foldLeft(0L)((r,c) => r + ways3(k-1,n-c)*ways3(1, c))
}

def waysM(k: Int, m: Int, n: Int) : Long = {
 // ncalls += 1
  if (n < 4) 0L else if (k == 1) n-m else List.range(m+1,n-m).foldLeft(0L)((r,c) => r + waysM(k-1,m,n-c)*waysM(1,m,c))
}

  def f(n: Int) = {
    var s = 2L + n-3
    List.range(0,n) foreach {
      b0 => 
	0 +: List.range(3, n-b0) foreach {
	  rf => 
	    List.range(1, (n-b0-rf)/4+1) foreach {
	      k => 
		val nn = (n-b0-rf)-4*k
	      val ns = ways(k, nn)
	      println("(b0, k, ns, rf) = " + (b0, k, nn, rf))
	      s += ns
	    }
	}
    }
  }

  def g(m: Int, n: Int) = {
    var s = 0L 
    List.range(0,n+1) foreach {
      b0 => 
	0 +: List.range(m, n-b0+1) foreach {	    
	  rf => 
	    val nn = (n-b0-rf)
	    //val nn = (n-b0)
	      if (nn >= m) {
	      List.range(1, n).takeWhile {
		k => 
		  val ns = cache.getOrElseUpdate((k, nn), waysM(k, m, nn))
		  //val ns = ways3(k, nn)
		  //println("(b0, k, nn, ns, rf) = " + (b0, k, nn, ns, rf))
		  //println("(b0, k, nn, ns) = " + (b0, k, nn, ns))
		  s += ns
		ns > 0
	      }	       
	      }
	      else if (b0 + rf == n) {
		//println("(b0, rf) = " + (b0, rf))
		s += 1
	      }
	}
    }
    s
  }

  def main(args: Array[String]) {
    //val n = args(0).toInt
    //println("res = " + g(10, n) + "\n ncalls = " + ncalls) 
    //println("res = " + g(3, 8))    
    //println("res = " + g(10, 57))
    var n = 50
    var r = g(50, n)
    while(r < 1000000) {
      n+=1
      r = g(50, n)
    }
    println("r = " + r + " n = " + n)  
  }
}
