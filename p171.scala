
object P171 {

  import scalax.io._
  import scalax.file.{ FileOps, Path, NotFileException }

//already computed for 
  //val pp = Set(1, 4, 9, 16, 25, 36, 49, 64, 81, 100)//, 

  val pp = Set(121, 144, 169, 196, 225, 256, 289, 324, 361, 400, 441, 484, 529, 576, 625, 676, 729, 784, 841, 900, 961, 1024, 1089, 1156, 1225, 1296, 1369, 1444, 1521)//, 1600)

  val lim = 20

  val indices = List.range(0, lim)

  val digits = List.range(0, 10)
 
  /**@returns the sum of the last 9 digits for each n of at most lim digits which satisfy (C);
    * (C) is: parikh image of n = lapp
    */
  def sum9old(lapp: Map[Int, Int]) = {
    val limnzeros = lim - lapp.map{_._2}.sum
    val symbols = lapp.filter{_._2 != 0}.keySet
    
  }

  def sum9(lapp: List[Int]): Int = {
    val limnzeros = lim - lapp.sum
    val choices = List.fill(limnzeros)(0) ::: digits.tail.map{d => List.fill(lapp(d-1))(d)}.reduceLeft(_++_)
    val sols = choices.permutations.toList//choices.combinations(9)
    println("sols.length = " + sols.length)
    //println("some sols = " + sols.take(10))
    sols.foldLeft(0)((r,c) => (r % 1000000000) + c.mkString.takeRight(9).toInt)
  }

  def comb0(n: Int, k: Int): Long = if (k == 0 || n == k) 1 else if (k == 1) n else comb0(n-1, k) + comb0(n-1, k-1)

  def comb(n: Int, k: Int) = {
    if (k == 0) 1 else {
      val c = Array.fill(n+1, k+1)(0L)
      List.range(1, n+1) foreach {
	i => 
	  c(i)(1) = 1L*i
	if (i <= k) c(i)(i) = 1L
	val m = math.min(i, k)
	List.range(2, m+1) foreach {
	  j => 
	    c(i)(j) = c(i-1)(j-1) + c(i-1)(j)
	}
      }
      c(n)(k)      
    }
  }

  def ncombs0(apps: List[Int], m: Int) : Long = if (m < 2) 1 else comb(m, apps.head) * ncombs0(apps.tail, m - apps.head)

  def ncombs(apps: List[Int], m: Int, sol: Long) : Long = {
    if (apps.isEmpty) sol
    else ncombs(apps.tail, m - apps.head, comb(m, apps.head) * sol)
  }

  def enum(mapp: Map[Int,Int], cs: Int, l: Int) : Long = {
    if (l == lim/2 || mapp.isEmpty) {
      val mappNo0 = mapp - 0 //if (mapp.keySet(0)) mapp - 0 else mapp
      val rcombs = 1L * ncombs(mappNo0.map{_._2}.toList, lim-l, 1)
      val ret = (rcombs * cs) % 1000000000
      //println("mapp = " + mapp + " cs = " + cs + " rcombs = " + rcombs + " ret = " + ret)
      ret
    }
    else {
      val res = for(el <- mapp; 
		    val d = el._1; 
		    val napp = el._2;        
		    if (napp > 0)
		  ) yield enum(mapp.updated(d, napp - 1).filter{_._2 > 0}, cs*10 + d, l + 1)
      if (res.isEmpty) {
	println("mapp = " + mapp)
	0
      } 
      else res.sum//reduceLeft(_ % 1000000000 + _)
    }
  }
/*
  def enum(mapp: Map[Int,Int], cs: Int, l: Int) : List[(Int,String)] = {
    if (l == lim/2) {
      ncombs(mapp) * cs
    }
    else {
      val res = for(el <- mapp; 
		    val d = el._1; 
		    val napp = el._2; 
		    s <- enum(mapp.updated(d, napp - 1).filter{_._2 > 0}, ); 
		    if (napp > 0)
		  ) yield cs*10 + d
      res.toList
    }
  }
*/
  def enum0(mapp: Map[Int,Int]) : List[List[Int]] = {
    if (mapp.size == 1) {
      val d = mapp.toList(0)._1
      val n = mapp.toList(0)._2
      List(List.fill(n)(d))
    }
    else {
      val res = for(el <- mapp; 
		    val d = el._1; 
		    val napp = el._2; 
		    s <- enum0(mapp.updated(d, napp - 1).filter{_._2 > 0}); 
		    if (napp > 0)
		  ) yield s :+ d
      res.toList
    }
  }

  //m(a) is the nb of occurences of symbol a
  def f(m: Map[Int, Int]) = m.map{x => (x._2 * x._1 * x._1)}.sum
  
  def totalOcc(m: Map[Int, Int]) = m.map{_._2}.sum

  def msup(a: Int, v: Int, m: Map[Int, Int]) = math.min(lim - totalOcc(m), v - f(m)) + 1

  /**@return {(i1,...,ik) | \sum ij < l, f(m) = v for m.keys = sigma and m(aj) = ij}
    */ 
  def getCombi0(sigma: Set[Int], v: Int, csol: Map[Int, Int]) : Set[Map[Int, Int]] = {
    //if (sigma.isEmpty && f(csol) == v && csol.keySet == sigma.toSet) Set(csol)
    if (sigma.isEmpty && f(csol) == v) Set(csol)
    else {
      val res = for(a <- sigma; i <- List.range(0, msup(a, v, csol))) yield getCombi0(sigma - a, v, csol + (a -> i))
      res.flatten
    }
  }
  
  def getCombi1(sigma: Set[Int], csol: Map[Int, Int], vals: Set[Int]) : Set[Map[Int, Int]] = {
    //if (sigma.isEmpty && f(csol) == v && csol.keySet == sigma.toSet) Set(csol)
    if (sigma.isEmpty && pp(f(csol))) Set(csol)
    else {
      val res = for(a <- sigma; i <- List.range(0, msup(a, vals.max, csol))) yield getCombi1(sigma - a, csol + (a -> i), vals.map{v => v - i*a*a})
      res.flatten
    }
  }

  def getCombi(sigma: Set[Int], sols: Set[(Int, Map[Int, Int])]) : Set[(Int, Map[Int, Int])] = {
    //println("sigma = " + sigma + " sols = " + sols)
    //if (sigma.isEmpty && f(csol) == v && csol.keySet == sigma.toSet) Set(csol)
    //if (sigma.isEmpty) sols.filter{csol => pp(f(csol._2))}
    if (sigma.size == 1) {     
      val el = sigma.head
      val el2 = el*el
      val res = pp.filter{p => p >= el2}.map{p => (1 to p/el2).map{i => (p - i*el2, Map(el -> i))}}.flatten
//      println("sigma = " + sigma + " res = " + res)
      res
    }
    else for(a <- sigma; 
	     val a2 = a*a; 
	     cs <- (getCombi(sigma - a, sols)).filter{s => totalOcc(s._2) < lim};
	     val cm = cs._2;
	     val cv = cs._1;
	     val max = if (cv >= a2) cv/a2 else 0;
	     i <- (0 to max)
	   ) yield (cv - i*a2, cm + (a -> i))
  }

  def getCombi2(sigma: Set[Int], sols: Set[Map[Int, Int]]) : Set[Map[Int, Int]] = {
    //if (sigma.isEmpty && f(csol) == v && csol.keySet == sigma.toSet) Set(csol)
    //println("sigma = " + sigma + " sols = " + sols)
    //if (sigma.size == 1) pp.map{v => List.range(0, v/(sigma.head*sigma.head)).map{i => Map(sigma.head -> i)}}.flatten
    if (sigma.isEmpty) {
      val ret = sols.filter{s => pp(f(s))}
      if (!ret.isEmpty) println("sols = " + ret.foldLeft("")(_+"\n"+_))
      ret
		}
    else for(a <- sigma; 
	     //cs <- Map[Int,Int]() + (getCombi(sigma - a, sols)).filter{s => totalOcc(s) < lim}; 
	     cs <- (getCombi2(sigma - a, sols)).filter{s => totalOcc(s) < lim} + Map[Int,Int](); 
	     val cf = f(cs); 
	     val a2 = a*a; 
	     //val fpp = pp.filter{p => p >= cf && (p - cf) % a2 == 0}; 
	     val fpp = pp.filter{p => p >= cf};
	     val max = if (fpp.isEmpty) 0 else fpp.max;
	     i <- List.range(0, max / a2)
	   ) yield cs + (a -> i)
  }

  def ways(s : Int) = {
    var res = 0L
    var fs = 0L
    var napps = 0
    for(
      a0 <- s to 0 by -81;
      val napp9 = (s - a0)/81;      
      a1 <- a0 to 0 by -64;
      val napp8 = (a0 - a1)/64;
      a2 <- a1 to 0 by -49;
      val napp7 = (a1 - a2)/49;
      a3 <- a2 to 0 by -36;
      val napp6 = (a2 - a3)/36;
      a4 <- a3 to 0 by -25;
      val napp5 = (a3 - a4)/25;
      a5 <- a4 to 0 by -16; 
      val napp4 = (a4 - a5)/16;
      a6 <- a5 to 0 by -9;
      val napp3 = (a5 - a6)/9;
      a7 <- a6 to 0 by -4;
      val napp2 = (a6 - a7)/4;
      val napp1 = s - (napp2*4 + napp3*9 + napp4*16 + napp5*25 + napp6*36 + napp7*49 + napp8*64 + napp9*81);
      if (napp1 + napp2 + napp3 + napp4 + napp5 + napp6 + napp7 + napp8 + napp9 <= lim)
    ) {
	//res += 1L
	val napps = List(napp1, napp2, napp3, napp4, napp5, napp6, napp7, napp8, napp9)
	val mapps = (List.range(0,9).map{i => (i + 1 -> napps(i))} :+ (0 -> (lim - napps.sum))).toMap.filter{_._2 != 0}
	//val mapps = (List.range(0,9).map{i => (i + 1 -> napps(i))}).toMap.filter{_._2 != 0}
	if (s < 36) println("napps: " + napps + "\n" + mapps.toList.sortBy{_._1})
	val combis = enum(mapps, 0, 0)
	//println("combis : " + combis)//.foldLeft("")(_+"\n"+_))
	//fs += sum9(List(napp1, napp2, napp3, napp4, napp5, napp6, napp7, napp8, napp9))

        fs += combis % 1000000000
    }
    fs
  }
 
  def main(args: Array[String]) {
    //val sigma = Set(1,2,3,4,5)
    val sigma = digits.tail.toSet
/*
	val res = getCombi(sigma, pp(0) , Map[Int, Int]())
	println("res.size = " + res.size)
	res foreach {
	r => 
	  println(r) 
      }
*/
  val file = "/media/ubuntuPart2/docs/topcoder/outp171.txt"
  val out: Output = Resource.fromFile(file)

/*
  val res1 = getCombi1(sigma, Map[Int, Int](), Set(1, 4, 9, 16)).toList.sortBy{x => f(x)}
  res1 foreach {
      l => 
	val elS = l.toList.sorted.foldLeft("")((r,c) => (r + ", (" + c._1 + ", " + c._2 + ")")).drop(2)
	
      //out.write(f(l._2) + " " + elS + "\n")
      println(f(l) + " " + elS + "\n")
      //out.write(f(l) + " " + elS + "\n")
      //sum9(r)
    }  
  println( "Time (s): " + ( System.currentTimeMillis() - start ) / 1000. )

  start = System.currentTimeMillis
*/

    //for (i <- (0 to 20); j <- (0 to i); if (comb(i, j) != comb0(i, j))) println("wrong:" + (i, j))

    var sf = 0L

    //pp.toList.takeRight(2).sorted.foreach { 
    val pps = pp.toList.sorted
    pps foreach {
    p => 
     //val p = 4
     val start = System.currentTimeMillis
      val rp = ways(p)
      sf += rp
      sf = sf % 1000000000
      println("p = " + p + " rp = " + rp + " sf = " + sf)
     println( "Time (s): " + ( System.currentTimeMillis() - start ) / 1000. )
    }
/*
  val res = getCombi(sigma, pp.map{p => (p, Map[Int, Int]())}).map{_._2}.filter{x => pp(f(x))}.toList.groupBy{x => f(x)}.toList.sortBy{_._1}
  println( "Time (s): " + ( System.currentTimeMillis() - start ) / 1000. )
    res foreach {
      el => 
	println("nsols for " + el._1 + " = " + el._2.length + ":")
	el._2 foreach {
	  l =>
	    val elS = l.toList.sorted.foldLeft("")((r,c) => (r + ", (" + c._1 + ", " + c._2 + ")")).drop(2)	
	  //out.write(f(l._2) + " " + elS + "\n")
	  //println(f(l) + " " + elS + "\n")
	  out.write(f(l) + " " + elS + "\n")
	  //sum9(r)
	}
    }
    */ 
  }
}
