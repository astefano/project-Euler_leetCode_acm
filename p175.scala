/*
*Stern's diatomic series (A002487)
* number of alternating bit sets in n (see Finch).
* number of ways of writing n as a sum of powers of 2, each power being used at most twice (the number of hyperbinary representations of n) [Carlitz; Lind]
*
  *
  *
  *
  *OBS: the last try is findInStern-Brocol2. for the input params from the pr it takes ages... see the c code, p175.c, runs in no time. 
  *
  * 
 */
object P175 {

  import scala.collection.mutable._

  def binRepr(n: BigInt) = {
    def binReprAux(n: BigInt, res: List[Int]) : List[Int] = {	
      if (n==0) res
      else binReprAux(n>>1, res:+(n&1).toInt)
    }
    binReprAux(n, List()) 
  }

  //from repr n = 111001 get  5,4,3,0 (i.e. n = 2^5 + 2^4 + 2^3 + 2^0)
  def binPowers(n: BigInt) = {
    val binr = binRepr(n)
    (binr.zipWithIndex.filter(_._1!=0) map {_._2}).reverse
  }

  def shortenedBinExpr(binP: List[Int]) = {
    def aux(binP: List[Int], res: List[Int]) : List[Int] = binP match {
      case Nil => res
      case h::Nil => {
	//println("h " + h)
	(1 + res.head) +: res.tail
      }
      case h1::h2::t => {
	//println("h1 = " + h1 + " h2 = " + h2 + " t = " + t + " res = " + res)
	if (h1 == (h2 + 1)) aux(h2 :: t, (res.head + 1) +: res.tail) else aux(h2 :: t, 0 +: (h1 - h2 - 1) +: (res.head + 1) +: res.tail) 
      }
    }
    aux(binP, List(0)).reverse
  }

  def getNfromBinPow(l: List[Int]) = l.foldLeft(0)(_+math.pow(2,_).toInt)

  def verif(n: Long) = getNfromBinPow(binPowers(n)) == n 

  /** the number of ways in which n can be written as a sum of powers of 2 which may repeat at most twice.
    * e.g.: f(10) = 5: |{8+2, 8+1+1, 4+4+2, 4+4+1+1, 4+2+2+1+1}|
    * f(n) = 1 + \Sum_{i\in I, (i-1)\not\inI, i > 0} f(n - 2^i + 2^{i-1})
    */ 
  def fNaive(cand: List[Int]) = {
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
    //println("res = " + res)
    res
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

  def findSmallestN(powLim: Int, depth: Int) = {
    var f = new HashMap[List[Int], Int]()
    //init f[p] = p + 1 
    List.range(0, powLim) foreach {
      r => 
        f.update(List(r,r), r + 1)
	List.range(r + 1, powLim) foreach {
	  p =>
	    f.update(List(p,r), (p - r) * (r + 1) + r)
	}
    }
    var j = 0
    while(j < depth) {
      val nf = new HashMap[List[Int], Int]()
      //println("at j = " + j + " f.keys = " + f.keys.toList)
      f.keySet.toList.sortBy(l => l(0)) foreach {
	lk => 
	  //lk has 2 elements or more	      
	  val p2 = lk(0)		 
	  val lkT = lk.tail
	  //println("lk " + lk)
	  //add f([p2,p2,lk]) = f([p2,lk])
	  val nlkEq = p2 +: lk
	  val nvEq = f(p2 +: lkT)
	  nf.update(nlkEq, f(lk))
	  //println("addedEq " + nlkEq)	  
	  val p3 = lk(1)
	  List.range(p2 + 1, powLim) foreach {
	    p1 => 	  
	      val nlk = p1 +: lk
	      var nv0 = 0
	      if (p2 > 0) {
		var prev = p2
		//println("start sub from lk = " + lk)
		val subLk = lk.dropWhile{
		  el => 
		    val aux = prev
		    //if (nlk == List(4,3,2)) println("el = " + el + " aux = " + aux)
		    if (el == aux) prev = el - 1
		  el == aux
		}	     
	        //if (nlk == List(4,3,2)) println("adding " + nlk + " subLk = " + subLk + " prev = " + prev)
		if (subLk != List() && prev >= subLk.head) nv0 = f.getOrElse(prev +: subLk, fNaive(prev +: subLk))
		if (subLk == List() && prev >= 0) nv0 = 1 + prev
	      }	     
	      val nv = (p1 - p2) * f(lk) + nv0
	      if (nv == 123456789) println("found " + nlk) 
	      nf.update(nlk, nv)
	      //check
	      /* 
	      val fEffVal = fEff(nlk)
	      val fN = fNaive(nlk)
	      if (fN != nv) {       	
		val n = getNfromBinPow(nlk)
		println("diff res for " + nlk + ", i.e., n = " + n + " fEff = " + fEffVal + " nv = " + nv + " f = " + fN + " lkT = " + lkT)
	      }
	     */
	  }
      }
      /* forget f: when computing nf(l) with l of length k we only need l of length k - 1
       *TODEL and k - 2 (when we need f[pp] we transform it into f[p+1])
       */ 
      //fprev = f
      f = nf
      j = j + 1
    }    
    println("f.size = " + f.size + " some keys = " + f.keySet.toList.takeRight(10))
  }

import math.BigInt._

/* f(2n) = f(n) + f(n-1)
 * f(2n+1) = f(n)
 * f(n) the number of ways to write n as sums of i^2 with at most 2 repetitions
 */
def fEff2(value: BigInt) = {
  val nbits = value.bitLength
  var sumstd = 1L
  var sumcarry = 1L  
  // for each bit
  var i = 1
  while (i < nbits) {
      // find if its a 1
      if (value.testBit(i))
        sumstd += sumcarry
      else
        sumcarry += sumstd
    i = i + 1
    }
  sumstd
  //println("Result: " + sumstd)
  }

  def findInSternBrocot(lim: Int) = {
    var l = (0,1)
    var r = (1,0)
    val wanted = (987654321/9, 123456789/9)
//    val wanted = (800001,100000)
//    val wanted = (4,2)
    val fw = 1.*wanted._1/wanted._2
    println("fw = " + fw)
    var found = false
    //var path = ""
    var i = 1
    var powLevel = BigInt(1)
    var n = BigInt(0)
    var m = wanted
    while (!found && i < lim) {
      m = (l._1 + r._1, l._2 + r._2)
      val fm = 1.*m._1/m._2
      val oldPowLevel = powLevel
      powLevel = powLevel<<1
      if (m._1 == wanted._1 && m._2 == wanted._2) found = true
      //if (fm == fw) found = true
      else if ( fm < fw) {
	//go right
	l = m
	//path = path :+ 'R'
	n += powLevel
      }
     else {
       //go left
       r = m
       //path = path :+ 'L'
	n += oldPowLevel
     }
      i += 1
    }
    println("Result: n = " + n + " found = " + found + " at i = " + i)// + " m = " + m + "\nbin(n) = " + binRepr(n) + " path = " + path)
    //path
    n
  }

  def findInSternBrocot2(mv: Int, nv: Int) = {
    // 109739369 13717421
    var m = mv//123456789/9 = 13717421
    var n = nv//987654321/9 = 109739369
    var res = 0
    var path = ""
    var powLevel = 1
    var rightC = 0
    var leftC = 0
    var prevLeft = false
    var cLeft = false
    var resEnc = ""
    while (m != n) {
      val oldPowLevel = powLevel
      val prevLeft = cLeft
      powLevel = powLevel<<1
      if (m < n) { 
	path = path + 'L' 
	n = n - m
	res += oldPowLevel
	leftC += 1
	cLeft = true
	if (!prevLeft) {
	  resEnc += rightC 
	  resEnc += ','
	  rightC = 0 
	}
      }
      else {
	path = path + 'R' 
	m = m - n
	res += powLevel
	rightC += 1
	cLeft = false
	if (prevLeft) {
	  resEnc += leftC
	  resEnc += ','
	  leftC = 0
	}
      }
    }
    //add last iter
    if (cLeft)
      resEnc = resEnc + leftC + ",1"
    else 
      resEnc += rightC + 1
    if (resEnc(0) == '0') resEnc = resEnc.tail
    println("Result: res = " + res + "\nbin(res+1) = " + binRepr(res+1) + " path = " + path + " resEnc = " + resEnc + " reversed = " + resEnc.reverse)
    resEnc.reverse
  }

  def main(args: Array[String]) {
    val start = System.currentTimeMillis
    //val lim = math.pow(10,args(0).toInt).toInt
    //findInSternBrocot(lim)
    findInSternBrocot2(args(0).toInt, args(1).toInt)
    /*
    val max = args(0).toInt
    val depth = args(1).toInt
    findSmallestN(max, depth)
    */ 
/*
    val nd1 = args(0).toInt
    val nd2 = args(1).toInt
    val limit = BigInt(List.range(0,nd2).foldLeft("1")((r,c)=>(r + "0")))
    var n = BigInt(List.range(0,nd1).foldLeft("1")((r,c)=>(r + "0")))
    while(fEff2(n) != 13717421 && n < limit) n += 1
    println(" n = " + n)
*/ 
/*
    var i = math.pow(10,16).toLong
    val nMax = math.pow(10,17).toLong
    var found = false
    while (i < nMax && !found) {
      val n1 = binPowers(i)
      val n2 = binPowers(i-1)
      val r1 = fEff(n1) 
      val r2 = fEff(n2) 
      if (r1 == 123456789 && r2 == 987654321) {
	println("found " + i)
	found = true 
      }
      i = i + 1
    }
*/ 
/*    List.range(3551, 5550) map {
      i => 
      if( f(i) != fEff(binPowers(i)) ) println("i = " + i)
    }
*/ 
    println( "Time (s): " + ( System.currentTimeMillis() - start ) / 1000. )
  }

}
