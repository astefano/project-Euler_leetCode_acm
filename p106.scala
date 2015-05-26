object P106{

  def smaller(s1: Set[Int], s2: Set[Int]) = {
    val sl1 = s1.toList.sorted
    val sl2 = s2.toList.sorted
    def smallerR(l1: List[Int], l2: List[Int]): Boolean = {
      if (l1 == List()) true
      else {
	if (l2 == List()) false
	else if (l1.head <= l2.head) smallerR(l1.tail, l2.tail) else smallerR(l1, l2.tail)
      }
    }
    smallerR(sl1, sl2)
  }

  def genCand(s: List[Int]) = {    
    println("s = " + s)
    val all = (s.toSet.subsets.toSet - Set[Int]()).toList
    var stop = false
    var n = 0
    var n2 = 0
    var cand = Set[(Set[Int], Set[Int])]()
    all.foreach {
      b => 
	all.filter(x => ((x & b) == Set())).foreach {
	  c => 
	      if (b.size < c.size) n += 1
	      if (b.size == c.size) n2 += 1	      
	      if (b.size == c.size && (b.size > 1 || c.size > 1)) {
		if (!( smaller(b, c) || smaller(c, b) || cand((c,b)))) {
		  cand = cand + ((b, c))
		  //println((b.toList.sorted, c.toList.sorted) + " +")
		}
		//else println((b.toList.sorted, c.toList.sorted) + " -")
	    }
	}
    } 
    println("n = " + (n + (n2/2)) + " don't know = " + cand.size)// + " cand = " + cand.foldLeft("")(_+"\n"+_))
    cand
  }  

/*
  def prune(cand: Set[(Set[Int], Set[Int])]) = {
    var ncand = List[(Set[Int], Set[Int])]()
    cand foreach {
      cp => 
	//if (cand == List() || (cand != List() && !cand.exists(p => (p._1 == b && smallerEq(p._2, c)) || (p._2 == c && smallerEq(b, p._1)))))
	//add to cand p1 < p2 if not (b <= p1 < p2 = c || b = p1 < p2 == c)
	if (!(cand - cp).exists(p => (p._1 == cp._1 && smaller(p._2, cp._2)) || (p._2 == cp._2 && smaller(cp._1, p._1))))
	  ncand = ncand :+ cp
    }
    println(" don't know = " + ncand.length + " ncand = " + ncand.foldLeft("")(_+"\n"+_))
    ncand
  }
*/

  def main(args: Array[String]) {
    val start = System.currentTimeMillis

    val cand = genCand(List.range(1, 13))
//    prune(cand)

    println( "Time (s): " + ( System.currentTimeMillis() - start ) / 1000. )
  }

}
