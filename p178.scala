object P178 {

  val lim = math.pow(10,11).toLong  

  def test = {
    var n = 9876543210L 
    while (n <= lim) {
	val ns = n.toString
	val nsl = ns.toList
	if (ns.toSet.size == 10) {
	  val r = (1 to ns.length-1).dropWhile{i => math.abs(nsl(i) - nsl(i-1)) == 1 }
	  if (r.isEmpty) print(n + ", ")
	}	
	n += 1
    }
  }

  def main(args : Array[String]) {
    val start = System.currentTimeMillis

    val l = 1026

    //the max length of numbers
    val n = args(0).toInt

    //the nb of digits used
    //val nd = args(1).toInt
    
    /* nstep(j)(k)(i) the number of k digits ending in i and
     * having support j, i.e., the digits used encode j
     * j is from 1 to 2^10; the positions of the bits set to 1
     * are the digits used 
     */
    val nstep = Array.fill(l, n+2, 10)(0L)
    /* init: the nb of numbers of 1 digit ending in i,
     * i.e., using digit i, i.e., with j the nb with bit i set to 1
     */ 
    List.range(1,10) foreach {
      i =>
	nstep(1 << i)(1)(i) = 1L
    }

    List.range(2, n+1) foreach {
      k =>
	List.range(0, 10) foreach {
	  i =>
	    //select the repr. with bit i set to 1
	    //List.range(1,1024).filter(j => (j & (1 << i)) != 0) foreach {
	    List.range(1,1024).filter(j => (j & (1 << i)) != 0) foreach {
	      j =>   
		//add the vals from the previous level coming from repr j with or without bit i set to 1
		val jWithouti = j & ~(1 << i)		
		//if (j > 1021 && i < 3 && k > 9) println("k =" + k + " i = " + i + " j = " + j + " binj = " + j.toBinaryString + " binjNoi = " + jWithouti.toBinaryString + " " + nstep(jWithouti)(k-1).toList)
		if (i != 0) nstep(j)(k)(i) += nstep(j)(k-1)(i-1) + nstep(jWithouti)(k-1)(i-1)
  	        if (i != 9) nstep(j)(k)(i) += nstep(j)(k-1)(i+1) + nstep(jWithouti)(k-1)(i+1)
	    }
	}
/*      //treat the case of numbers ending at 0 (resp. at 9) at level k-1 separately 
      List.range(1,1024).filter(j => (j & (1 << 0)) != 0) foreach {
	j => 
	  val jWithout0 = j & ~(1 << 0)
	  if (j == 1023) println("k=" + k + " " + jWithout0 + " " + nstep(jWithout0)(k-1).toList) 
	  nstep(j)(k)(0) += nstep(j)(k-1)(1) + nstep(jWithout0)(k-1)(1)
      }
      List.range(1,1024).filter(j => (j & (1 << 9)) != 0) foreach {
	j => 
	  nstep(j)(k)(9) += nstep(j)(k-1)(8) + nstep(j & ~(1 << 0))(k-1)(8)
      }
*/
    }
    var s = 0L
    (1 to n) foreach { 
      v => 
      s+=nstep(1023)(v).sum
    }
    val result = s
    println(result)

    //test

    println( "Time (s): " + ( System.currentTimeMillis() - start ) / 1000. )

  }
}
