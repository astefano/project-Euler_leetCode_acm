object limacon {

  def isSafe0(queens: Set[(Int, Int)], q: (Int, Int)) =  queens.find(qx => qx._1 == q._1 || qx._2 == q._2) == None

  def isSafe(queens: Set[(Int, Int)], queen: (Int, Int)) = 
    queens forall (q => !inCheck(queen, q))
  
  def inCheck(q1: (Int, Int), q2: (Int, Int)) = 
    q1._1 == q2._1 ||  // same row
    q1._2 == q2._2 //||  // same column
    //(q1._1 - q2._1).abs == (q1._2 - q2._2).abs // on diagonal 

  def queens(n: Int) = {
    def placeQueens(k: Int): List[Set[(Int, Int)]] = {
      if (k == 0) List(Set())
      else {
	for {
	  sol <- placeQueens(k-1)
	  col <- (1 to n)
	  q = (k, col) 
	    if (isSafe(sol, q))
	} yield sol + q
      } 
    }
//    placeQueens(n)
//  }

  def printQ(n: Int, sol: Set[(Int, Int)]) = {
    var s = ""
    (1 to n) foreach {
      i => 
	(1 to n) foreach {
	  j => 
	    s += (if (sol contains (i, j)) "Q " else "* ")
	}
	s += "\n"
    }
    println(s)
  }

  def printQ2(n: Int, sol: Set[((Int, Int), Int)]) = {
    var s = ""
    (1 to n) foreach {
      i => 
	(1 to n) foreach {
	  j => 
	    s += (if (sol contains ((i, j), 1)) "1 " else if (sol contains ((i, j), 2)) "2 " else if (sol contains ((i, j), 3)) "3 " else "  ")
	}
	s += "\n"
    }
    println(s)
  }

  def toMString(n: Int, sol: Set[((Int, Int), Int)]) = {
    var s = ""
    List((1,1), (1,2), (1,3), (1,4), (1,5), 
	 (2,5), (3,5), (4,5), (5,5), 
	 (5,4),	(5,3), (5,2), (5,1), 
	 (4,1), (3,1), (2,1),
	 (2,2), (2,3), (2,4),
	 (3,4), (4,4), 
	 (4,3), (4,2),
	 (3,2), (3,3)) foreach {
	   x => 
	     s += (if (sol contains (x, 1)) "1" else if (sol contains (x, 2)) "2" else if (sol contains (x, 3)) "3" else "")
	 }
    //println(s)
    s
  }

  def findSol(n: Int, sols: Set[Set[(Int, Int)]]) = {
    for(s1 <- sols.filter(x => (x contains (1,1))); s2 <- sols - s1; s3 <- sols - s1 - s2; val ns = ((s1.map(x => (x, 1)) union s2.map(x => (x, 2))) union s3.map(x => (x, 3))); val s = toMString(n, ns); if ((s1 intersect s2).isEmpty && (s2 intersect s3).isEmpty && (s1 intersect s3).isEmpty && s.replaceAll("123","")=="") ) yield ns
  }

  def main(args: Array[String]) {
    val start = System.currentTimeMillis
    val sols = queens(5)
    var i = 0
    /*sols.foreach{
      s => 
	i += 1
	println("Sol " + i + ":")
	printQ(5, s)
    }*/
    findSol(5, sols.toSet).foreach{s => printQ2(5, s)}
    println( "Time (s): " + ( System.currentTimeMillis() - start ) / 1000. )
  }

}
