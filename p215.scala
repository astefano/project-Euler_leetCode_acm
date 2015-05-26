/* Consider the problem of building a wall out of 21 and 31 bricks
 (horizontalvertical dimensions) such that, for extra strength, the
 gaps between horizontally-adjacent bricks never line up in
 consecutive layers, i.e. never form a "running crack".
*/ 
object P215 {

  def ways1(n: Int): Set[String] = {
    if (n < 2) Set()
    else if (n == 2) Set("2")
    else if (n == 3) Set("3")
    else ways1(n-2).map{el => List.range(0,el.length).map{i=>el.patch(i,"2",0)}.toSet}.flatten ++ 
	 ways1(n-3).map{el => List.range(0,el.length).map{i=>el.patch(i,"3",0)}.toSet}.flatten
  }

  def testWays1(n: Int): Int = {
    if (n <= 1) 0
    else if (n == 2) 1
    else if (n == 3) 1
    else testWays1(n-3) + testWays1(n-2)
  }

  def valid(s: String, t: String) = {
    val ls = s.length
    val lt = t.length
    val min = math.min(ls,lt)
    var partialSums1 = Set[Int]()
    var partialSums2 = Set[Int]()
    var sum1 = 0
    var sum2 = 0
    List.range(0,min-1) foreach{
      i => 
	sum1 += s(i)-'0'
      sum2 += t(i)-'0'
      partialSums1 += sum1
      partialSums2 += sum2
    }
    if (ls != lt) {
      if (min == ls) 
	List.range(min-1, lt-1) foreach {
	  i =>
	    sum2 += t(i)-'0'
	  partialSums2 += sum2	  
	} 
      else
	List.range(min-1, ls-1) foreach {
	  i => 
	    sum1 += s(i)-'0'
	  partialSums1 += sum1
	} 
    }
    //println("ps = " + partialSums1 + " pt = " + partialSums2)
    (partialSums1 intersect partialSums2).isEmpty
  }

  def waysF(n: Int, k: Int): Long = {
    val cand = ways1(n).toList 
    def aux(next: List[String], rem: Int): Long = {
      //println("rem = " + rem + " next = " + next)
      if (next == Set()) 0
      else if (rem == 1) next.size.toLong
      else {
	val res = next.map{
	  x => 
	    val ncand = cand.filter(y => y != x && valid(x, y))
	  //println("x = " + x + " ncand = " + ncand)
	  aux(ncand, rem - 1)
	}
      //println("res = " + res)
      res.foldLeft(0L)(_+_)
      }
    }
    if (k == 1) cand.length
    else aux(cand, k) 
  }

  def waysDP(n: Int, k: Int): BigInt = {
    val cand = ways1(n).toList
    //println("cand =" + cand)
    val l = cand.length
    val ll = List.range(0,l)

    /* validM(i,j) = true if valid(cand(i),cand(j))
     * so validM(i,i) = false
     * and validM is symmetric
     */ 
    var validM: Array[Array[Boolean]] = Array.ofDim(l,l) 
    ll foreach {
      i => 
	validM(i)(i) = false
	List.range(i+1,l) foreach {
	  j =>
	    validM(i)(j) = valid(cand(i), cand(j))
	    validM(j)(i) = validM(i)(j)
	    //if (validM(i)(j) == true) println((cand(i), cand(j)))
	}
    }

    //println("validM = " + validM.toList.foldLeft("")(_+"\n"+_.toList))    

    //m(i,j) = the nb of walls of height i+1 ending with cand(j)
    var m: Array[Array[BigInt]] = Array.ofDim(k,l)
    ll foreach { 
      j => 
	m(0)(j) = BigInt(1)
    }
    List.range(1, k) foreach {
      i => 
	 ll foreach { 
	  j => 	
	    val r = ll.filter{l => validM(j)(l)}
	    //println("j = " + j + " r = " + r)
	    m(i)(j) = r.foldLeft(BigInt(0))((r,c)=>(r + m(i-1)(c)))
	 }     
    }
    //println(m.toList.foldLeft("")(_+_.toList))
    m(k-1).foldLeft(BigInt(0))(_+_)
  }

  def main(args: Array[String]) {

    val nr = args(0).toInt
    val nc = args(1).toInt

    val start = System.currentTimeMillis
/*
    List.range(17,22) foreach {
      nr => 
	List.range(2,7) foreach {
	  nc => 
	    val res1 = waysF(nr, nc)    
	    val res2 = waysDP(nr, nc)    
	    println("(nr, nc) = " + (nr, nc) + "res1 = " + res1 + " res2 = " + res2 + " diff = " + (res1 - res2))
	}
    }
    */
    val res = waysDP(nr, nc)    
    println(" res = " + res)
    
    println( "Time (s): " + ( System.currentTimeMillis() - start ) / 1000. )
  }
}

