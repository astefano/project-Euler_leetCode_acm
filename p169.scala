object P169 {

  import scala.collection.mutable.Stack

  //decompose 10^k into a sum \sigma 2^{i_j} with i_j distinct
  def dec2(k: Int) : List[Int] = { 
    if (k==1) List(1,3) 
    else {
      if (k % 2 == 0) {
	val r = dec2(k/2)
	simpl(mult(r,r).sorted)
      }
      else {
	val r1 = dec2(k/2)
	val r2 = dec2(k - k/2)
	simpl(mult(r1,r2).sorted)
      }
    }
  }

  //l1 and l2 are the lists of exponents of 2
  def mult(l1: List[Int], l2: List[Int]) = {
    val n1 = l1.length
    val n2 = l2.length
    val range1 = List.range(0,n1)
    val range2 = List.range(0,n2)
    for(i1 <- range1; i2 <- range2) yield l1(i1)+l2(i2)
  }

  //simplifies a sorted list of exponents of 2, i.e., each 2 equal exponents p are replaced by 1 exp p+1 until all exp are distinct
  def simpl(l: List[Int]) : List[Int] = {
    //println(" l = " + l)
    l match {
      case h1::h2::tail => { 
	//println("h1 = " + h1 + " h2 = " + h2 + " tail = " + tail); 
	if (h1 == h2) simpl(((h1+1)::tail).sorted) else h1::simpl(h2::tail) 
      }
      case nil => { 
	//println("nil"); 
	nil }
    }
  }

  //check if 10^k = \sum 2^l(i)
  def check(k: Int) = math.pow(10,k) == dec2(k).foldLeft(0.)((r,c)=>(r+math.pow(2,c)))

  def binRepr(n: Int) = {
    def binReprAux(n: Int, res: List[Int]) : List[Int] = {	
      if (n==0) res
      else binReprAux(n>>1, res:+(n&1))
    }
    binReprAux(n, List()) 
  }

  /** the number of ways in which the list cand can be rewritten 
    * e.g.: f([31]) = | [31], [211], [300], [2200], [21100] |
    */ 
  def f(cand: List[Int]) = {
    var visited = Set[List[Int]]()
    val candStack = new Stack[List[Int]]
    candStack.push(cand)
    //println("pushed: " + cand)
    var i = 0
    while (candStack != Stack()) {
      val c = candStack.pop
      val cl = c.length
      //println("popped: " + c)
      visited = visited + c
      i = 0
      while(i < cl - 1) {
	if (c(i) > 0 && c(i) > c(i+1) + 1) {
	  val nc = ((c.take(i):+(c(i)-1)):+(c(i)-1)):::c.drop(i+1)
	  if (!visited(nc)) { 
	    //println("at " + i + " pushed: " + nc)
	    //println("pushed: " + nc)
	    candStack.push(nc)
	  }
	}	
	i = i + 1
      }
      //handle last case
      if (c(cl - 1) > 0) {
	val nc = (c.take(cl-1):+(c(cl-1)-1)):+(c(cl-1)-1)
	  if (!visited(nc)) { 
	    //println("pushed: " + nc)
	    candStack.push(nc)
	  }
      }
    }
    val res = visited.size
    //println("visited = " + visited.foldLeft("")(_+"\n"+_) )
    println("res = " + res)
    res
  }

  def fEffBuggyOrNot(l: List[Int]) : Long = {
    if (l.length == 1) l(0) + 1L
    if (l.length == 2) {
      if (l(1) == 0) l(0) 
      else ( l(0) - l(1) ) * ( l(1) + 1L ) + l(1)
    }
    else 1L * ( l(0) - l(1) ) * fEff(l.tail) + fEff( (l(1) - 1) +: l.drop(2) )
  }

 def fEff(l: List[Int]) : Long = {
    if (l.length == 1) l(0) + 1L
    else if (l.length == 2) {
      if (l(0) == l(1)) l(0) + 1L
      //else if (l(1) == 0 && l(0) > 0) l(0) 
      else ( l(0) - l(1) ) * ( l(1) + 1L ) + l(1)
    }
    else {
      /*val aux = 0
      else if (l(1) - 1L == l(2) && ( (l.drop(3) == List()) || (l.drop(3)!=List && l(3) != l(2)) )) 1L * ( l(0) - l(1) ) * fEff(l.tail) + fEff( (l(1) - 1) +: l.drop(2) ) - 1L
	   else if (l(1) - 1L > l(2)) 1L * ( l(0) - l(1) ) * fEff(l.tail) + fEff( (l(1) - 1) +: l.drop(2) )
		else 1L * ( l(0) - l(1) ) * fEff(l.tail) + aux
		*/ 
      if (l(0) == l(1)) 1L + fEff((l(0) - 1) +: l.drop(2)) 
      else {
	var prev = l(1)
	val subLk = l.tail.dropWhile{
	  el => 
	    val aux = prev
	    if (el == aux) prev = el - 1
	    el == aux
	}	     
	var nv0 = 0L
	if (subLk != List() && prev >= subLk.head) nv0 = fEff(prev +: subLk)
	if (subLk == List() && prev >= 0) nv0 = 1 + prev	
	1L * ( l(0) - l(1) ) * fEff(l.tail) + nv0
      }
    }
  }

  def main(args: Array[String]) {
    val start = System.currentTimeMillis
    val n = args(0).toInt
    val repr = dec2(n)
    println("repr: " + repr + " 10^" + n + " check: " + check(n))
    /* val res = List.range(12, 13) map { 
      i => 
	val repr = dec2(i)
	println("repri " + repr)
	val r1 = fEff(repr.reverse)
	val r2 = f(repr.reverse)
	if (r1 != r2) println("repr = " + repr + " r1 = " + r1 + " r2 = " + r2)
    } */
    println("Result = " + fEff(repr.reverse))
    println( "Time (s): " + ( System.currentTimeMillis() - start ) / 1000. )
  }
}
