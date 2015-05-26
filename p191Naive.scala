object P191 {

  val na = 2
  val nl = 1

  //'0' is 'O', '1' is 'L', '2' is 'A'
  def valid(s: String) = {
    if (s.contains("222") || s.filter(_=='1').length>1) false
    else true
  }

  def main(args: Array[String]) {
    //val ndays = args(0).toInt
    var nsols = 0L
    var sols = Set[String]()
    ('0' to '2') foreach {
      d1 => 
	('0' to '2') foreach {
	  d2 => 
	    ('0' to '2') foreach {
	      d3 => 
		('0' to '2') foreach {
		  d4 => 
		    ('0' to '2') foreach {
		      d5 => 
			('0' to '2') foreach {
			  d6 => 
			    ('0' to '2') foreach {
			      d7 => 
				val cand = ""+d1+d2+d3+d4+d5+d6+d7
			      if (valid(cand)) {
				sols += cand
				nsols += 1
			      }
			    }
			}
		    }
		}
	    }
	}
    }

    val (lSols, noLSols) = sols.map{_.replace('0','O').replace('2','A')}.partition(_.contains("1"))
    println("noL:" + noLSols.size + "\n" + noLSols.reduceLeft(_+" "+_))
    val (a3sols, pslsols) = lSols.map{_.replace("1","")}.partition(_.contains("AAA"))

    println("solsWithL: valid n-1 strings: " + pslsols.size + "\n" +  pslsols.reduceLeft(_+" "+_))
    println("solsWithL: from 3 aaas: " + a3sols.size + "\n" +  a3sols.reduceLeft(_+" "+_))
    println("nsols" + nsols)
  }
}

