/* https://code.google.com/codejam/contest/3214486/dashboard
*/
object digits {

  import scalax.io._
  import scalax.file.{ FileOps, Path, NotFileException }

  val digits = Map((1, List(0,1,1,0,0,0,0)), (2, List(1,1,0,1,1,0,1)), (3, List(1,1,1,1,0,0,1)), 
		   (4, List(0,1,1,0,0,1,1)), (5, List(1,0,1,1,0,1,1)), (6, List(1,0,1,1,1,1,1)), 
		   (7, List(1,1,1,0,0,0,0)), (8, List(1,1,1,1,1,1,1)), (9, List(1,1,1,1,0,1,1)), 
		   (0, List(1,1,1,1,1,1,0)))

  val digitsR = digits.map(x => (x._2, x._1)).toMap.withDefaultValue(-1)

  def turnOn(l: List[Int], p: Set[Int]) = l.zipWithIndex.map(lip => if(p contains lip._2) 1 else lip._1)
  def turnOff(l: List[Int], p: Set[Int]) = l.zipWithIndex.map(lip => if(p contains lip._2) 0 else lip._1)

  def intersectD(s: Array[List[Int]], d: Int) = {
    //i assume s has at least one element
    val n = s(0).length
    val indices = (0 to n-1)
    val i0 = indices.filter(p => s(0)(p) == d).toSet
    s.tail.foldLeft(i0)((r,c) => r intersect indices.filter(p => c(p) == d).toSet)    
  }

  def unionD(s: Array[List[Int]], d: Int) = {
    //i assume s has at least one element
    val n = s(0).length
    val indices = (0 to n-1)
    val i0 = indices.filter(p => s(0)(p) == d).toSet
    s.tail.foldLeft(i0)((r,c) => r union indices.filter(p => c(p) == d).toSet)    
  }

  def find(s: Array[List[Int]]) = {
    //the broken segments are the zeros common to all s(i)
    val broken = intersectD(s, 0)
    //println("broken = " + broken)
    //the working segments are the ones in each s(i)
    val working = unionD(s, 1)
    assert((broken union working) == (0 to 6).toSet)
    val candidates = broken.subsets.toList
    //the length of the series
    val ns = s.length
    val n = s(0).length
    /* any digit obtained from s(0) by turning on broken segments is ok as the first element
     * as long as it is bigger than ns */
    val inits = for(c <- candidates; val d = digitsR(turnOn(s(0), c)); if (d >= ns)) yield d
    //println("inits = " + inits.toList + " cand = " + candidates)
    var last = -1
    val remi = inits.dropWhile {
      first => 
	last = -1 
        val rems = (1 to ns-1).dropWhile {
	  j => 
	    var found = false
	    candidates.dropWhile {
	      c => 
		val cd = digitsR(turnOn(s(j), c))
		//println(first + " c = " +  c + " cd = " + cd)
		/* we found a subset of broken segments which if turned on
		 * form a digit at distance j from first */
		found = (first == cd + j) 
		!found  
	    }
	  found
	}
      //if for all s(1), ..., s(n-1) we found a solution then we're done
      if (rems.isEmpty) last = first - ns
      //println("rems = " + rems + " last = " + last)
      !rems.isEmpty
    }
    if (remi.isEmpty) "ERROR!"
    else turnOff(digits(last), broken).mkString
  }

  def findAll(s: Array[List[Int]]) = {
    //the broken segments are the zeros common to all s(i)
    val broken = intersectD(s, 0)
    //println("broken = " + broken)
    val working = unionD(s, 1)
    //println("working = " + working)
    val candidates = broken.subsets.toList
    //the length of the series
    val ns = s.length
    val n = s(0).length
    /* any digit obtained from s(0) by turning on broken segments is ok as the first element
     * as long as it is bigger than ns */
    val inits = for(c <- candidates; val d = digitsR(turnOn(s(0), c)); if (d != -1)) yield d
    val initsL = inits.map(x => List(x))
    //println("inits =" + inits)
    def findAllR(k: Int) : List[List[Int]] = {
      if (k == 1) initsL
      else {
	for {
	  sol <- findAllR(k-1)
	  c <- candidates
	  val nd = digitsR(turnOn(s(k-1), c))
	  //care that if the last el is 0 than its succ is 9
	  if ((nd != -1 && nd == sol(0) - 1) || (sol(0) == 0 && nd == 9))
	} yield nd :: sol 
      }
    }
    val res = findAllR(ns)
    //println("res = " + res)
    if (res.isEmpty || res.length > 1 || ...) "ERROR!"   
    else {
      //see that the last element in the serie might be a 0 so it's succ is 9
      val nEl = if (res(0)(0) == 0) 9 else res(0)(0)-1
      turnOff(digits(nEl), broken).mkString
    }
  }

  def main(args: Array[String]) {
    val start = System.currentTimeMillis

    //val file = "testdigits.in"   
    val file = "A-small-practice.in"   
    //val file = "C-large-practice.in"   
    val rf: Input = Resource.fromFile(file)

    //val fileOut = "testdigits.out"
    val fileOut = "A-small-practicel.out"   
    //val fileOut = "C-large-practice.out"   
    val rfo: Output = Resource.fromFile(fileOut)

    val lines = rf.lines().filter(_!="")
    val nlines = lines.size
    var out = ""

    val ncases = lines(0).toInt
    (0 to ncases-1) foreach {
      i => 
	val ns = lines(i+1).split(" ")
	val n = ns(0).toInt
	val s = ns.tail.map(_.toList.map(_.toInt-'0'))
	//println("n = " + n + " s = " + s.toList)
	out += "Case #"+(i+1)+": " + findAll(s) + "\n"
    }
    //println(out)
    rfo.write(out)
    println( "Time (s): " + ( System.currentTimeMillis() - start ) / 1000. )
  }

}
