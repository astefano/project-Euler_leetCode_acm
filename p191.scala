object P191 {

  val na = 2
  val nl = 1

  def main(args: Array[String]) {
    val ndays = args(0).toInt
    var s = 0L
    val narr = Array.ofDim[Long](ndays+1,na+1,nl+1)
    narr(0)(0)(0)=1
    (1 to ndays) foreach {
      d => 
	(0 to na) foreach {
	  a => 
	    (0 to nl) foreach {
	      l => 
		if (a == 0) {
		  s = 0
		  (0 to na) foreach {
		    x => 
		      s += narr(d-1)(x)(l)
		  }
		  if (l > 0) {
		    (0 to na) foreach {
		      x => 
			s += narr(d-1)(x)(l-1)
		    }
		  }
		}
		  else s += narr(d-1)(a-1)(l)
	      narr(d)(a)(l) = s
	    }
	}
    }
    println(narr(ndays).map{x => x.toList}.toList)
    println("res= " + narr(ndays).flatten.sum)
  }

}
