object P103{

  def genS(n: Int): List[Int] = if (n == 1) List(1) else { 
    val sprev = genS(n-1) 
    val l = sprev.length
    val b = sprev(l/2)
    b +: sprev.map{_+b} 
  }

  def valid(s: Set[Int]): Boolean = { 
    //println("Info: s = " + s.toList.sorted + " s.sum = " + s.sum)
    val all = (s.subsets.toSet - Set[Int]()).toList
    var stop = false
    val rem = all.dropWhile {
      b => 
	val r0 = all.filter(x => ((x & b) == Set()))
	//println("r0 = " + r0)
	val r1 = r0.dropWhile {
	  c => 
	  val sat = !(b.sum == c.sum || (b.size > c.size && b.sum < c.sum))
	  //if (!sat) println("counterex b = " + b + " c = " + c)
	  (sat)
	}
	//if(r1 != List()) println("b = " + b + " r1 = " + r1)
      (r1 == List())
    } 
    //println("rem = " + rem)
    rem == List()
  }  

  def cpL(l : List[List[Int]]) : List[List[Int]] = l match {
      case Nil => List(List())
      case xl :: r => for (rl <- cpL(r).filter(c => valid(c.toSet)); x <- xl; if (!rl.toSet(x) && rl.sum + x < 269)) yield rl :+ x
  }    

  def findS(s: Set[Int]) = {
    val ss = s.sum
    val sl = s.toList.sorted
    val aux0 = cpL(sl.map{el => List.range(el-4, el+4)}.toList)
    val aux = aux0.map{_.sorted}.toSet
    val cands = aux.filter(x => x.toSet.size == s.size && x.sum < 269).toList.sortBy(_.sum)
    println("cands = " + cands.length + cands.map{x => (x.sorted, x.sum)}.foldLeft("")(_+"\n"+_))
    val r = cands.dropWhile(sc => !valid(sc.toSet))
    if (r != List()) r.head else Nil
  }

  def main(args: Array[String]) {
    val start = System.currentTimeMillis

    val s = genS(7).toSet
    val rs = findS(s)
    if (rs != Nil) println("rs = " + rs.mkString + " res " + valid(rs.toSet))
    //println(valid(Set(8, 14, 17, 19, 20, 21)))
    println( "Time (s): " + ( System.currentTimeMillis() - start ) / 1000. )
  }

}
