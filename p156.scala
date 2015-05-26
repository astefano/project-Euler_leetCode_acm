object P156 {

  def counts(bound : Long, d : Int) = List.range(0, bound+1).foldLeft("")(_+_).filter(_==(d+48).toChar).length

  def countsR(bound : Long, d : Int, res : Long) : Long = if (bound > 0) countsR(bound - 1, d, res + bound.toString.filter(_==(d+48).toChar).length) else res
  
  /* c(d) is the number of occurences of d in the list 0...lim
   * s(d) is the sum of those intermediary values i for which c(d) is equal to i */
  def countsItS(lim : Long) = {
     val start = System.currentTimeMillis
     val s = Array.fill(10)(0L)
     val c = Array.fill(10)(0L)    
     var i = 0L
     while (i <= lim) {
       var tmp = i
       var visited = Set[Int]()
       while(tmp != 0) {
	 val d = (tmp%10).toInt
	 c(d) += 1L
	 tmp = tmp/10
	 if (c(d) == i) visited += d
       }
       val d1 = visited.filter(d => c(d) == i)
       if (d1 != Set()) {
       d1 foreach {
	 d => 
	   s(d) += i
	   if (i > math.pow(10,8).toInt) 
	     println("i = " + i + " visited = " + d1 + " c(" + d + ") =" + c(d)  + " s(" + d + ") = " + s(d))
       }
       }
      i += 1
    }
    println("c = " + c.toList + " s = " + s.toList + " total = " + s.foldLeft(0L)(_+_))
    println( "Time (s): " + ( System.currentTimeMillis() - start ) / 1000. )
  }

  countsItS(10000000000L)

/*
1000000001:  List(0, 21247726731, 1597982965, 4087599868, 5499999879, 0, 0, 0, 0, 0) total = 32433309443
                     22786974071
2000000000:  List(0, 23469948952, 1597982965, 4087599868, 5499999879, 0, 0, 0, 0, 0) total = 34655531664
10000000000: List(0, 23469948952, 3820205187, 7420933201, 9944444323, 5555555555, 112579066567, 66179794800, 418357960971, 9999999999)
10000000001: List(0, 23469948952, 3820205187, 7420933201, 9944444323, 5555555555, 112579066567, 66179794800, 418357960971, 9999999999) total = 657327909555

List(8888888908, 10000000003, 10000000000, 10000000000, 10000000000, 10000000000, 10000000000, 10000000000, 10000000000, 10000000000) 
10000000001: List(0, 22786974071, 11868991481, 14215999875, 15499999885, 10000000000, 115783999905, 68131008510, 419040935919, 10000000000) total = 687327909646

List(_,  900000003, 900000000, 900000000, 900000000, 900000000, 900000000, 900000000, 900000000, 900000000) 
1000000000: List(0, 19029862961, 798991481, 3715999875, 4999999885, 0, 0, 0, 0, 0) total = 28544854202

1000000001: List(_, 21675862961, 1868991481, 4215999875, 5499999885, 0..) //ocaml

1 -> 10000000001 
2 -> 10000000000 
3 -> 10000000000 
4 -> 10000000000 
5 -> 10000000000 
6 -> 10000000000 
7 -> 10000000000 
8 -> 10000000000 
9 -> 10000000000 
10000000001:       22786974071 + 11868991481 + 14215999875 + 15499999885 + 10000000000 + 115783999905 + 68131008510 + 419040935919 + 10000000000 = 687327909646
                   11868991481  
                   14215999875
		   15499999885
		   10000000000
		 6 115783999905
		   68131008510
		 8 419040935919
		   10000000000
		   *
  *
  * 1 -> 110000000003 22786974071    22786974071
    2 -> 110000000000 73737982962    96524957033
    3 -> 110000000000 372647999625   469172956658
    4 -> 110000000000 741999999540   1211172956198
    5 -> 110000000000 100000000000   1311172956198
    6 -> 110000000000 2434703999430  3745876955628
    7 -> 110000000000 1876917059570  5622794015198
    8 -> 110000000000 15312327487352 20935121502550
    9 -> 110000000000 360000000000   21295121502550
*/
  
  /* returns the nb of occur of digit d in i */
  def noccur(i : Int, d : Int) = i.toString.filter(_==(d+48).toChar).length

  /* returns the sublist of numbers which have k digits of d */
  def getNO(l : List[Int], k : Int, d : Int) = l.filter{i => noccur(i, d) == k}

  def main(args: Array[String]) {

    val start = System.currentTimeMillis
    val lim = args(0).toInt
    val digits = List.range(0, 10)
    /* m(k, j, d) is the number of k-digit numbers with j-occurences of d
     * so m(k, k, d) = 1
     * m(k, 0, d) = 8*9^{k-1}
     * general case: m(k, j, d) is the sum of the:
     *      - number of (k-1)-digit numbers with j-occur of d where we can insert an arbitrary digit diff of d:
     *                              a. in k-1 places with d \in {0, 1, .., 9} - {d}
     *                              b. in the front, thus d \in {1,..,9} - {d}
     *         so (9(k-1)+8)*m(k-1, j, d)        
     *      - number of (k-1)-digit numbers with (j-1)-occur of d where we can insert another d in k places
     *         so k*m(k-1, j-1, d)
     */ 
    val m = Array.fill(lim+1, lim+1, 10)(0L)
    digits foreach {
      d => 
	List.range(1, lim+1) foreach {
	  k => 
	    m(k)(k)(d) = 1
	    m(k)(0)(d) = 8*math.pow(9, k-1).toLong
	}
    }

    digits foreach {
      d => 
	List.range(2, lim+1) foreach {
	  k => 
	    List.range(1, k) foreach {
	      j => 
		m(k)(j)(d) = (9L*k-1)*m(k-1)(j)(d) + k*m(k-1)(j-1)(d)
	    }
	}
    }

    println(m(2)(1)(1))

    println( "Time (s): " + ( System.currentTimeMillis() - start ) / 1000. )
  }
}
