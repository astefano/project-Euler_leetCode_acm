object P373 {

import scalax.io._
import scalax.file.{ FileOps, Path, NotFileException }

  def main(args: Array[String]) {
    //p373(args(0).toInt)
    p373T(args(0).toInt)
    val r = List.range(840,845) map { x => p373Naive(x) - p373T(x) }
    println(" r = " + r)
    //p373Naive(args(0).toInt)
    //p373Tri(args(0).toInt)
  }

  def gcd(a: Int, b: Int): Int = if(b == 0) a else gcd(b, a % b)

  def lcm(a: Int, b: Int) = a * (b / gcd(a, b))

  def binomial(n: Int, j: Int) : Long = {
    if (j == 0 || n == j) 1
    else if (j == 1) n
    else binomial(n-1, j-1) + binomial(n-1, j)
  }

  def enc3(a: Int, b: Int, c: Int) = binomial(a+b+c+2, 3) + binomial(a+b+1,2) + a

  def enc(a: Int, b: Int) = (a+b)*(a+b+1)/2 + b

def getRadius(l: List[Double]) = {
  val a = l(0)
  val b = l(1)
  val c = l(2)
  val abc = 1L*a*b*c
  val s = (a + b + c)/2
  val area = math.sqrt(1L*s*(s-a)*(s-b)*(s-c))
  val radius = abc / (4L*area)
  //println("abc = " + abc + " area = " + area + " radius = " + radius)
  radius
}


  def p373T(lim: Int) = {
    val start = System.currentTimeMillis
    val file = "/media/ubuntuPart2/docs/topcoder/pythagorean-triples.txt"
    val rf: Input = Resource.fromFile(file)
    val lines = rf.lines().filter(_!="")
    val nlines = lines.size
    println("n lines = " + nlines)
    var s = 0L
    var clines = 0
    val linesSplit = lines.toList map { li => li.trim.split(" ").filter(_!="") map {_.toInt} }
    val linesFilter1 = linesSplit.filter(_.length >= 3)
    val triples = linesFilter1.filter(t => t(2) <= 2*lim)
    val n = triples.length
    println("triples length = " + n)
    var i = 0 
    var j = 1
    val visited: Array[Array[Set[(Int,Int)]]] = new Array(n,n)
//    val visitedIso: Array[Set[(Int,Int)]] = new Array(n)
    val visitedIsoAux: Array[Set[(Int,Int)]] = new Array(n)
    var visitedIsoBases = Set[Double]()
    //val visitedI: Array[Array[Int]] = new Array(10*lim,10*lim)
    //val visitedJ: Array[Array[Int]] = new Array(10*lim,10*lim)
    var abcL = List[List[Double]]()
    var nabcL = List[List[Double]]()      
    var abc = List[Double]()
    var abcIso = Set[List[Double]]()      
    var abcIso2 = Set[List[Double]]()      
/*    
    var abcL = List[List[Int]]()
    var nabcL = List[List[Int]]()      
    var abc = List[Int]()
*/ 
    var iso = 0
    while (i < n) {
      val t = triples(i).toList.take(3)      
      /* right triangle
       * a = 2R b = 2R p/q
       * c = 2R ( sqrt(q^2-p^2) / q )
       * SO R = kq with k <= lim/q, i.e., the sum of radii is q*kc*(kc+1)/2
       * with kc = lim/q
       */ 
      val kcRight = lim/t(2)
      val nsr = t(2) * kcRight * (kcRight + 1)/2
      s = s + nsr 
      if (kcRight > 0) println("right triangle: " + List(t(0), t(1), t(2)).sorted + " nsr = " + nsr)
      abcL =  List(1.*t(0), 1.*t(1), 1.*t(2)).sorted +: abcL 
      if ( getRadius(List(2.*t(0), 2.*t(1), 2.*t(2))) != 1.*t(2)) println("Error: right angle")
      /* isosceles triangle
       * a = b = 2R p/q or a = b = 2R r/q
       * c = 2R ( 2p sqrt(q^2-p^2) / q^2 )
       * SO R = kq^2 with k <= lim/q^2, i.e., the sum of radii is 2*q^2*kc*(kc+1)/2
       * with kc = lim/q^2
       * we have twice because there are 2 isosceles triangles possible
       */ 	
      val kcIsosceles = lim/(t(2)*t(2))
      val nsIso = t(2) * t(2) * kcIsosceles * (kcIsosceles + 1)
      s = s + nsIso
      if (kcIsosceles >= 1) {
	visitedIsoBases = visitedIsoBases + 2.*t(0)*t(1)
	visitedIsoAux(iso) = Set((t(1),t(2)),(t(0),t(2)))
	abcIso = abcIso + List(1.*t(2)*t(0), 1.*t(2)*t(0), 2.*t(0)*t(1)).sorted + List(1.*t(2)*t(1), 1.*t(2)*t(1), 2.*t(1)*t(0)).sorted
	println("iso added at t = " + t + " : " + List(t(2)*t(0), t(2)*t(0), 2*t(0)*t(1)).sorted + " and " + List(t(2)*t(1), t(2)*t(1), 2*t(1)*t(0)).sorted + " nsIso = " + nsIso)
	if (getRadius(List(2.*t(2)*t(0), 2.*t(2)*t(0), 4.*t(0)*t(1))) != 1.*t(2)*t(2) || getRadius(List(2.*t(2)*t(1), 2.*t(2)*t(1), 4.*t(1)*t(0))) != 1.*t(2)*t(2)) println("error isoscel radius")
	iso = iso + 1
      }
      i = i + 1 
    }
    val visitedIso = visitedIsoAux.takeWhile(_!=null)
    println("abcIso has size = " + abcIso.size + " and elements "  + abcIso.foldLeft("")(_+"\n"+_))
    println("visitedIso has length = " + visitedIso.toList.length + " and elements "  + visitedIso.toList.foldLeft("")(_+"\n"+_))
    println("visitedIsoBases = " + visitedIsoBases)
    i = 0 
    while (i < n) {
      val t = triples(i).toList.take(3)
      val p1 = t(0)
      val r1 = t(1)
      val q1 = t(2)
      //visited(i)(i) = Set((p1,q1), (r1,q1))
      //abcIso = abcIso + List(1.*r1/q1, 1.*r1/q1, 1.*p1/q1).sorted + List(1.*p1/q1, 1.*p1/q1, 1.*r1/q1).sorted
      //visited(i)(i) = Set((),())
      /* the general case, we work with 2 triples, t and tp, t = (p1,r1,q1), tp = (p2,r2,q2). We get 4 subcases, depending on c: 
       * I.  c = 2R (p1r2 + r1p2) / q1q2, we shorten p1r2 + r1p2 = factor1
       * I.1. a = 2R p1/q1 and b = 2R p2/q2
       * I.2. a = 2R r1/q1 and b = 2R r2/q2
       * II. c = 2R (p1p2 + r1r2) / q1q2, we shorten p1p2 + r1r2 = factor2
       * II.1. a = 2R p1/q1 and b = 2R r2/q2
       * II.2. a = 2R r1/q1 and b = 2R p2/q2
       * Let di = gcd(factori, q1q2), fri = factori/di, qri = q1q2/di. It follows that R = k * lcm(q1,q2,qri).
       * Obs1. the expr of c simplifies to: c = 2R fri / qri
       * Obs2. to check that the triangles in each case are not isosceles, because R is invariant, we only need to consider the ratios.
       */
      j = i + 1
      while (j < n) {
	val tp = triples(j).toList.take(3)
	val p2 = tp(0)
	val r2 = tp(1)
	val q2 = tp(2)
	val q1q2 = t(2)*tp(2)
	val mq1q2 = lcm(t(2), tp(2))
	/*careful, you might need to work with LONG for 10^7*/
	val p1q2 = t(0)*tp(2)
	val q1p2 = t(2)*tp(0) 
	val r1q2 = t(1)*tp(2)
	val q1r2 = t(2)*tp(1) 
	val p1p2 = t(0)*tp(0)
	val p1r2 = t(0)*tp(1)
	val r1r2 = t(1)*tp(1)
	val r1p2 = t(1)*tp(0)
	//val maxt2 = math.max(t(2),tp(2))
	//val mint2 = math.min(t(2),tp(2))
	/*CASE I.*/
	val factor1 = p1r2 + r1p2 
	val factor2 = r1r2 + p1p2
	val d1 = gcd(q1q2, factor1)
	val d2 = gcd(q1q2, factor2)
	val qr1 = q1q2 / d1
	val fr1 = factor1 / d1
        val l1 = lcm(t(2), lcm(tp(2), qr1))
	//val l1 = lcm(mq1q2, qr1)
	val kc1 = lim / l1
        val ns1 = l1 * kc1 * (kc1 + 1) / 2
	val qr2 = q1q2 / d2
	val fr2 = factor2 / d2
	val l2 = lcm(mq1q2, qr2)
	//val l2 = lcm(t(2), lcm(tp(2), qr2))
	val kc2 = lim / l2
	val ns2 = l2 * kc2 * (kc2 + 1) / 2
	val a1 = 1.*p1/q1
	val a2 = 1.*r1/q1
	val b1 = 1.*p2/q2
	val b2 = 1.*r2/q2
	val c1 = 1.*fr1/qr1
	val c2 = 1.*fr2/qr2
	if(kc1 > 0 || kc2 > 0) {	  
	  if (kc1 > 0 && visited(i)(j) == null) visited(i)(j) = Set((fr1,qr1))
	  if (kc2 > 0 && visited(i)(j) == null) visited(i)(j) = Set((fr2,qr2))
	  if (kc1 > 0 && visited(i)(j) != null && !visited(i)(j)((fr1,qr1))) visited(i)(j) = visited(i)(j) + ((fr1,qr1))
	  if (kc2 > 0 && visited(i)(j) != null && !visited(i)(j)((fr2,qr2))) visited(i)(j) = visited(i)(j) + ((fr2,qr2)) 
	  //cond for triangle not isosceles
	  //CASE I.1. 
	  val notIsoCI1 = (a1 != b1 && b1 != c1 && c1 != a1)
	  //CASE I.2. 
	  val notIsoCI2 = (a2 != b2 && b2 != c1 && c1 != a2)
	  //CASE II.1.
	  val notIsoCII1 = (a1 != b2 && b2 != c2 && c2 != a1)
	  //CASE II.2.
	  val notIsoCII2 = (a2 != b1 && b1 != c2 && c2 != a2)
	    if (kc1 > 0 && d1 == 1) {
	      abc = List(l1*a1,l1*b1,l1*c1)
	      if (notIsoCI1) {
		s = s + ns1
		abcL = abc.sorted +: abcL
		println("abc1 = " + abc + " t = " + t + " tp = " + tp + " (fr1,qr1) = " + (fr1,qr1) + " at (i,j) = " + (i,j) + " kc1 = " + kc1 + " R = " + l1 + " ns1 = " + ns1)		
		if (getRadius(abc.map{_*2.}) != l1) println("error radius")
	      } //else abcIso2 = abcIso2 + abc.sorted
	      abc = List(l1*a2,l1*b2,l1*c1)
	      if (notIsoCI2) {
		s = s + ns1
		abcL = abc.sorted +: abcL
		println("abc1 = " + abc + " t = " + t + " tp = " + tp + " (fr1,qr1) = " + (fr1,qr1) + " at (i,j) = " + (i,j) + " kc1 = " + kc1 + " R = " + l1 + " ns1 = " + ns1)		
	      } //else abcIso2 = abcIso2 + abc.sorted
	    }	    
	    if (kc2 > 0 && d2 == 1) {
	      abc = List(l2*a1,l2*b2,l2*c2)
	      if (notIsoCII1) {
		s = s + ns2
		abcL = abc.sorted +: abcL
		println("abc2 = " + abc + " t = " + t + " tp = " + tp + " (fr2,qr2) = " + (fr2,qr2) + " at (i,j) = " + (i,j) + " kc2 = " + kc2 + " R = " + l2 + " ns2 = " + ns2)		 
		if (getRadius(abc.map{_*2.}) != l2) println("error radius")
	      } //else abcIso2 = abcIso2 + abc.sorted
	      abc = List(l2*a2,l2*b1,l2*c2)
	      if (notIsoCII2) {
		s = s + ns2
		abcL = abc.sorted +: abcL
		println("abc2 = " + abc + " t = " + t + " tp = " + tp + " (fr2,qr2) = " + (fr2,qr2) + " at (i,j) = " + (i,j) + " kc2 = " + kc2 + " R = " + l2 + " ns2 = " + ns2)		
		if (getRadius(abc.map{_*2.}) != l2) println("error radius")
	      } //else abcIso2 = abcIso2 + abc.sorted
	    }	    
	    if(d1 > 1 || d2 > 1) {
	      var k = 0
	      var repeatedI1 = false
	      var repeatedI2 = false
	      var repeatedII1 = false
	      var repeatedII2 = false
	      while (k < i && (repeatedI1 == false || repeatedI2 == false || repeatedII1 == false || repeatedII2 == false)) {
		if ( d1 > 1 && kc1 > 0 && 
		    ( notIsoCI1 && ( (visited(k)(j) != null && visited(k)(j)((p1,q1))) || (visited(i)(k) != null && visited(i)(k)((p2,q2))) ) ) ) repeatedI1 = true
		if ( d1 > 1 && kc1 > 0 && 
		    ( notIsoCI2 && ( (visited(k)(j) != null && visited(k)(j)((r1,q1))) || (visited(i)(k) != null && visited(i)(k)((r2,q2))) ) ) ) repeatedI2 = true
		if ( d2 > 1 && kc2 > 0 && 
		    ( notIsoCII1 && ( (visited(k)(j) != null && visited(k)(j)((p1,q1))) || (visited(i)(k) != null && visited(i)(k)((r2,q2))) ) ) ) repeatedII1 = true
		if ( d2 > 1 && kc2 > 0 && 
		    ( notIsoCII2 && ( (visited(k)(j) != null && visited(k)(j)((r1,q1))) || (visited(i)(k) != null && visited(i)(k)((p2,q2))) ) ) ) repeatedII2 = true
		k = k + 1
	      }
	      if (k == i) {		
		// should i check iso here also?? 
		if (k == 0) k = 1
		//println("at (i,j) = " + (i,j) + " reached k = " + k + " visited = " + (visited map {_.toList}).toList.take(2))
		while (k < j && (repeatedI1 == false || repeatedI2 == false || repeatedII1 == false || repeatedII2 == false)) {
		  if (visited(i)(k) != null) {
		    if ((d1 > 1 && kc1 > 0 && !repeatedI1 && visited(i)(k)((p2,q2))) ) repeatedI1 = true
		    if ((d1 > 1 && kc1 > 0 && !repeatedI2 && visited(i)(k)((r2,q2))) ) repeatedI2 = true
		    if ((d2 > 1 && kc2 > 0 && !repeatedII1 && visited(i)(k)((r2,q2))) ) repeatedII1 = true
		    if ((d2 > 1 && kc2 > 0 && !repeatedII2 && visited(i)(k)((p2,q2))) ) repeatedII2 = true
		  }
		  k = k + 1
		}
		k = 0
		val visitedIsoL = visitedIso.toList.length
		/*
		 while (k < visitedIsoL && (repeatedI1 == false || repeatedI2 == false || repeatedII1 == false || repeatedII2 == false)) {
		    if (d1 > 1 && kc1 > 0 && !repeatedI1 &&
			( !notIsoCI1 && ( (a1 == b1 && visitedIso(k)((fr1,qr1))) || 
					  (b1 == c1 && visitedIso(k)((p1,q1))) || (c1 == a1 && visitedIso(k)((p2,q2))) ) ) ) repeatedI1 = true
		    if (d1 > 1 && kc1 > 0 && !repeatedI2 &&
			( !notIsoCI2 && ( (a2 == b2 && visitedIso(k)((fr1,qr1))) || 
				          (b2 == c1 && visitedIso(k)((r1,q1))) || (c1 == a2 && visitedIso(k)((r2,q2))) ) ) ) repeatedI2 = true
		    if (d2 > 1 && kc2 > 0 && !repeatedII1 && 
			( !notIsoCII1 && ( (a1 == b2 && visitedIso(k)((fr2,qr2))) || 
				           (b2 == c2 && visitedIso(k)((p1,q1))) || (c2 == a1 && visitedIso(k)((r2,q2))) ) ) ) repeatedII1 = true
		    if (d2 > 1 && kc2 > 0 && !repeatedII2 && 
			( !notIsoCII2 && ( (a2 == b1 && visitedIso(k)((fr2,qr2))) || 
				           (b1 == c2 && visitedIso(k)((r1,q1))) || (c2 == a2 && visitedIso(k)((p2,q2))) ) ) ) repeatedII2 = true
		  k = k + 1
		}*/
/*		    if (d1 > 1 && kc1 > 0 && !repeatedI1 &&
			( !notIsoCI1 && ( (a1 == b1 && visitedIsoBases(l1*c1)) || 
					  (b1 == c1 && visitedIsoBases(l1*a1)) || (c1 == a1 && visitedIsoBases(l1*b1)) ) ) ) repeatedI1 = true
		    if (d1 > 1 && kc1 > 0 && !repeatedI2 &&
			( !notIsoCI2 && ( (a2 == b2 && visitedIsoBases(l1*c1)) || 
				          (b2 == c1 && visitedIsoBases(l1*a2)) || (c1 == a2 && visitedIsoBases(l1*b2)) ) ) ) repeatedI2 = true
		    if (d2 > 1 && kc2 > 0 && !repeatedII1 && 
			( !notIsoCII1 && ( (a1 == b2 && visitedIsoBases(l2*c2)) || 
				           (b2 == c2 && visitedIsoBases(l2*a1)) || (c2 == a1 && visitedIsoBases(l2*b2)) ) ) ) repeatedII1 = true
		    if (d2 > 1 && kc2 > 0 && !repeatedII2 && 
			( !notIsoCII2 && ( (a2 == b1 && visitedIsoBases(l2*c2)) || 
				           (b1 == c2 && visitedIsoBases(l2*a2)) || (c2 == a2 && visitedIsoBases(l2*b1)) ) ) ) repeatedII2 = true
*/		 
       		if (d1 > 1 && kc1 > 0 && !repeatedI1) {
		  s = s + ns1
		  abc = List(l1*a1,l1*b1,l1*c1)
		  abcL = abc.sorted +: abcL
		  println("abcri1 = " + abc + " t = " + t + " tp = " + tp + " (fr1,qr1) = " + (fr1,qr1) + " at (i,j) = " + (i,j) + " R = " + l1 + " ns1 = " + ns1 + " isIso " + (!notIsoCI1))
		  if (getRadius(abc.map{_*2.}) != l1) println("error radius")
		}
		else {
		  abc = List(l1*a1,l1*b1,l1*c1)
		  if (notIsoCI1) {		  
		  if(abc.max < 100.) {		    
		    nabcL = abc.sorted +: nabcL
		    println("abcri1 = " + abc + " t = " + t + " tp = " + tp + " (fr1,qr1) = " + (fr1,qr1) + " at (i,j) = " + (i,j) + " not added")
		  }
		} else { println("abcISO " + notIsoCI1 + " not added " + abc + " repeatedI1 = " + repeatedI1 + " t = " + t + " tp = " + tp + " l1 = " + l1); abcIso2 = abcIso2 + abc.sorted }
		}
		if (d1 > 1 && kc1 > 0 && !repeatedI2) {
		  s = s + ns1
		  abc = List(l1*a2,l1*b2,l1*c1)
		  abcL = abc.sorted +: abcL
		  println("abcri2 = " + abc + " t = " + t + " tp = " + tp + " (fr1,qr1) = " + (fr1,qr1) + " at (i,j) = " + (i,j)  + " R = " + l1 + " ns1 = " + ns1 + " isIso " + (!notIsoCI2))
		  if (getRadius(abc.map{_*2.}) != l1) println("error radius")
		}
		else {
		  abc = List(l1*a2,l1*b2,l1*c1)
		  if (notIsoCI2){
		    if(abc.max < 100.) {		 
		      nabcL = abc.sorted +: nabcL
		      println("abcri2 = " + abc + " t = " + t + " tp = " + tp + " (fr1,qr1) = " + (fr1,qr1) + " at (i,j) = " + (i,j) + " not added")		
		    }
		} else { println("abcISO " + notIsoCI2 + " not added " + abc + " repeatedI2 = " + repeatedI2 + " t = " + t + " tp = " + tp + " l1 = " + l1); abcIso2 = abcIso2 + abc.sorted }
		}
		if (d2 > 1 && kc2 > 0 && !repeatedII1) {
		  s = s + ns2
		  abc = List(l2*a1,l2*b2,l2*c2)
		  abcL = abc.sorted +: abcL
		  println("abcrii1 = " + abc + " t = " + t + " tp = " + tp + " (fr2,qr2) = " + (fr2,qr2) + " at (i,j) = " + (i,j) + " R = " + l2 + " ns2 = " + ns2 + " isIso " + (!notIsoCII1))
		  if (getRadius(abc.map{_*2.}) != l2) println("error radius")
		}
		else {
		  abc = List(l2*a1,l2*b2,l2*c2)
		  if(notIsoCII1){		  
		  if(abc.max < 100.) {
		    nabcL = abc.sorted +: nabcL
		    println("abcrii1 = " + abc + " t = " + t + " tp = " + tp + " (fr2,qr2) = " + (fr2,qr2) + " at (i,j) = " + (i,j) + " not added")		
		  }
		} else { println("abcISO " + notIsoCII1 + " not added " + abc + " repeatedII1 = " + repeatedII1 + " t = " + t + " tp = " + tp + " l2 = " + l2); abcIso2 = abcIso2 + abc.sorted }
		}
		if (d2 > 1 && kc2 > 0 && !repeatedII2) {
		  s = s + ns2
		  abc = List(l2*a2,l2*b1,l2*c2)
		  abcL = abc.sorted +: abcL
		  println("abcrii2 = " + abc + " t = " + t + " tp = " + tp + " (fr2,qr2) = " + (fr2,qr2) + " at (i,j) = " + (i,j) + " R = " + l2 + " ns2 = " + ns2 + " isIso " + (!notIsoCII2))
		  if (getRadius(abc.map{_*2.}) != l2) println("error radius")
		}
		else { 
		  abc = List(l2*a2,l2*b1,l2*c2)
		  if (notIsoCII2) {
		    if(abc.max < 100.) {
		      nabcL = abc.sorted +: nabcL
		      println("abcrii2 = " + abc + " t = " + t + " tp = " + tp + " (fr2,qr2) = " + (fr2,qr2) + " at (i,j) = " + (i,j) + " not added") 
		    }	
		  } else { println("abcISO " + notIsoCII2 + " not added " + abc + " repeatedII2 = " + repeatedII2 + " t = " + t + " tp = " + tp + " l2 = " + l2); abcIso2 = abcIso2 + abc.sorted }
		}
	      }
	    }
	}	  
	j = j + 1
      }
      i = i + 1
    }
    println( "Result: " + s)
    val l = abcL.sortBy(l=>l(0))
    val temp = List.range(0,l.length-1).filter(i => l(i) == l(i+1))
    val repeated = (temp map {i => l(i)}).distinct
    println("abcL.length = " + abcL.length + " nabcL.length = " + nabcL.length +
	    " abcL.toSet.size = " + abcL.toSet.size + " nabcL.toSet.size = " + nabcL.toSet.size +
	    "\nnabc - abc = " + (nabcL.toSet -- abcL.toSet).toList.sortBy(l=>l(0)) + 
	    "\nrepeated in abcL = " + repeated +
	    "\n abcIso.size = " + abcIso.size + " abcIso2.size = " + abcIso2.size + 
	    "\n abcIso first 10 elem = " + abcIso.toList.take(10) + "\n abcIso2 first 10 elem = " + abcIso2.toList.take(10) + 
	    "\n iso diff = " + (abcIso2 -- abcIso))
    println( "Time (s): " + ( System.currentTimeMillis() - start ) / 1000. )
  }

  def p373T2(lim: Int) = {
    val start = System.currentTimeMillis
    val file = "/media/ubuntuPart2/docs/topcoder/pythagorean-triples.txt"
    val rf: Input = Resource.fromFile(file)
    val lines = rf.lines().filter(_!="")
    val nlines = lines.size
    println("n lines = " + nlines)
    var s = 0L
    var clines = 0
    val linesSplit = lines.toList map { li => li.trim.split(" ").filter(_!="") map {_.toInt} }
    val linesFilter1 = linesSplit.filter(_.length >= 3)
    val triples = linesFilter1.filter(t => t(2) <= 2*lim)
    val n = triples.length
    println("triples length = " + n)
    var i = 0 
    var j = 1
    val visited: Array[Array[Set[(Int,Int)]]] = new Array(n,n)
    while (i < n) {
      val t = triples(i).toList.take(3)      
      /* right triangle
       * a = 2R b = 2R p/q
       * c = 2R ( sqrt(q^2-p^2) / q )
       * SO R = kq with k <= lim/q, i.e., the sum of radii is q*kc*(kc+1)/2
       * with kc = lim/q
       */ 
      val kcRight = lim/t(2)
      s = s + t(2) * kcRight * (kcRight + 1)/2
      /* isosceles triangle
       * a = b = 2R p/q or a = b = 2R r/q
       * c = 2R ( 2p sqrt(q^2-p^2) / q^2 )
       * SO R = kq^2 with k <= lim/q^2, i.e., the sum of radii is 2*q^2*kc*(kc+1)/2
       * with kc = lim/q^2
       * we have twice because there are 2 isosceles triangles possible
       */ 	
      val kcIsosceles = lim/(t(2)*t(2))
      s = s + t(2) * t(2) * kcIsosceles * (kcIsosceles + 1)
      /* the general case, we work with 2 triples, t and tp, t = (p1,r1,q1), tp = (p2,r2,q2). We get 4 subcases, depending on c: 
       * I.  c = 2R (p1r2 + r1p2) / q1q2, we shorten p1r2 + r1p2 = factor1
       * I.1. a = 2R p1/q1 and b = 2R p2/q2
       * I.2. a = 2R r1/q1 and b = 2R r2/q2
       * II. c = 2R (p1p2 + r1r2) / q1q2, we shorten p1p2 + r1r2 = factor2
       * II.1. a = 2R p1/q1 and b = 2R r2/q2
       * II.2. a = 2R r1/q1 and b = 2R p2/q2
       * Let di = gcd(factori, q1q2), fri = factori/di, qri = q1q2/di. It follows that R = k * lcm(q1,q2,qri).
       * Obs1. the expr of c simplifies to: c = 2R fri / qri
       * Obs2. to check that the triangles in each case are not isosceles, because R is invariant, we only need to consider the ratios.
       */
      j = i + 1
      val p1 = t(0)
      val r1 = t(1)
      val q1 = t(2)
      while (j < n) {
	val tp = triples(j).toList.take(3)
	val p2 = tp(0)
	val r2 = tp(1)
	val q2 = tp(2)
	val q1q2 = t(2)*tp(2)
	val mq1q2 = lcm(t(2), tp(2))
	/*careful, you might need to work with LONG for 10^7*/
	val p1q2 = t(0)*tp(2)
	val q1p2 = t(2)*tp(0) 
	val r1q2 = t(1)*tp(2)
	val q1r2 = t(2)*tp(1) 
	val p1p2 = t(0)*tp(0)
	val p1r2 = t(0)*tp(1)
	val r1r2 = t(1)*tp(1)
	val r1p2 = t(1)*tp(0)
	//val maxt2 = math.max(t(2),tp(2))
	//val mint2 = math.min(t(2),tp(2))
	/*CASE I.*/
	val factor1 = p1r2 + r1p2 
	val factor2 = r1r2 + p1p2
	val d1 = gcd(q1q2, factor1)
	val d2 = gcd(q1q2, factor2)
	val qr1 = q1q2 / d1
	val fr1 = factor1 / d1
        val l1 = lcm(t(2), lcm(tp(2), qr1))
	val kc1 = lim / l1
        val ns1 = l1 * kc1 * (kc1 + 1) / 2
	val qr2 = q1q2 / d2
	val fr2 = factor2 / d2
	val l2 = lcm(mq1q2, qr2)
	val kc2 = lim / l2
	val ns2 = l2 * kc2 * (kc2 + 1) / 2
	if (l1 < 0 || l2 < 0 || d1 < 0 || d2 < 0 || fr1 < 0 || fr2 < 0) println("ERRRRRRRRRRRRRRRRRRROR ")
	val a1 = 1.*p1/q1
	val a2 = 1.*r1/q1
	val b1 = 1.*p2/q2
	val b2 = 1.*r2/q2
	val c1 = 1.*fr1/qr1
	val c2 = 1.*fr2/qr2
	if(kc1 > 0 || kc2 > 0) {	  
	  if (kc1 > 0 && visited(i)(j) == null) visited(i)(j) = Set((fr1,qr1))
	  if (kc2 > 0 && visited(i)(j) == null) visited(i)(j) = Set((fr2,qr2))
	  if (kc1 > 0 && visited(i)(j) != null && !visited(i)(j)((fr1,qr1))) visited(i)(j) = visited(i)(j) + ((fr1,qr1))
	  if (kc2 > 0 && visited(i)(j) != null && !visited(i)(j)((fr2,qr2))) visited(i)(j) = visited(i)(j) + ((fr2,qr2)) 
	  //cond for triangle not isosceles
	  //CASE I.1. 
	  val notIsoCI1 = (a1 != b1 && b1 != c1 && c1 != a1)
	  //CASE I.2. 
	  val notIsoCI2 = (a2 != b2 && b2 != c1 && c1 != a2)
	  //CASE II.1.
	  val notIsoCII1 = (a1 != b2 && b2 != c2 && c2 != a1)
	  //CASE II.2.
	  val notIsoCII2 = (a2 != b1 && b1 != c2 && c2 != a2)
	    if (kc1 > 0 && d1 == 1) {
	      if (notIsoCI1) 
		s = s + ns1
	      if (notIsoCI2) 
		s = s + ns1
	    }	    
	    if (kc2 > 0 && d2 == 1) {
	      if (notIsoCII1) 
		s = s + ns2
	      if (notIsoCII2) 
		s = s + ns2
	    }	    
	    if(d1 > 1 || d2 > 1) {
	      var k = 0
	      var repeatedI1 = false
	      var repeatedI2 = false
	      var repeatedII1 = false
	      var repeatedII2 = false
	      while (k < i && (repeatedI1 == false || repeatedI2 == false || repeatedII1 == false || repeatedII2 == false)) {
		if ( d1 > 1 && kc1 > 0 && notIsoCI1 && ( (visited(k)(j) != null && visited(k)(j)((p1,q1))) || (visited(i)(k) != null && visited(i)(k)((p2,q2))) ) ) repeatedI1 = true
		if ( d1 > 1 && kc1 > 0 && notIsoCI2 && ( (visited(k)(j) != null && visited(k)(j)((r1,q1))) || (visited(i)(k) != null && visited(i)(k)((r2,q2))) ) ) repeatedI2 = true
		if ( d2 > 1 && kc2 > 0 && notIsoCII1 && ( (visited(k)(j) != null && visited(k)(j)((p1,q1))) || (visited(i)(k) != null && visited(i)(k)((r2,q2))) ) ) repeatedII1 = true
		if ( d2 > 1 && kc2 > 0 && notIsoCII2 && ( (visited(k)(j) != null && visited(k)(j)((r1,q1))) || (visited(i)(k) != null && visited(i)(k)((p2,q2))) ) ) repeatedII2 = true
		k = k + 1
	      }
	      if (k == i) {		
		if (k == 0) k = 1
		//println("at (i,j) = " + (i,j) + " reached k = " + k + " visited = " + (visited map {_.toList}).toList.take(2))
		while (k < j && (repeatedI1 == false || repeatedI2 == false || repeatedII1 == false || repeatedII2 == false)) {
		  if (visited(i)(k) != null) {
		    if (d1 > 1 && kc1 > 0 && !repeatedI1 && visited(i)(k)((p2,q2)) ) repeatedI1 = true
		    if (d1 > 1 && kc1 > 0 && !repeatedI2 && visited(i)(k)((r2,q2)) ) repeatedI2 = true
		    if (d2 > 1 && kc2 > 0 && !repeatedII1 && visited(i)(k)((r2,q2)) ) repeatedII1 = true
		    if (d2 > 1 && kc2 > 0 && !repeatedII2 && visited(i)(k)((p2,q2)) ) repeatedII2 = true
		  }
		  k = k + 1
		}
		if (notIsoCI1 && d1 > 1 && kc1 > 0 && !repeatedI1) 
		  s = s + ns1
		if (notIsoCI2 && d1 > 1 && kc1 > 0 && !repeatedI2) 
		  s = s + ns1
		if (notIsoCII1 && d2 > 1 && kc2 > 0 && !repeatedII1)  
		  s = s + ns2		  
		if (notIsoCII2 && d2 > 1 && kc2 > 0 && !repeatedII2) 
		  s = s + ns2
	      }
	    }
	}	  
	j = j + 1
      }
      i = i + 1
    }
    println( "Result: " + s)
    println( "Time (s): " + ( System.currentTimeMillis() - start ) / 1000. )
  }

  def p373(lim: Int) = {
    val start = System.currentTimeMillis
    val file = "/media/ubuntuPart2/docs/topcoder/pythagorean-triples.txt"
    val rf: Input = Resource.fromFile(file)
    val lines = rf.lines().filter(_!="")
    val nlines = lines.size
    println("n lines = " + nlines)
    var s = 0L
    var clines = 0
    val linesSplit = lines.toList map { li => li.trim.split(" ").filter(_!="") map {_.toInt} }
    val linesFilter1 = linesSplit.filter(_.length >= 3)
    val triples = linesFilter1.filter(t => t(2) <= 2*lim)
    val n = triples.length
    println("triples length = " + n)
    var i = 0 
    var j = 1
    val factors: Array[Int] = new Array(10*lim)
    //val triangles: Array[(Int,Int)] = new Array(lim+1)
    var abcL = List[List[Int]]()
    var nabcL = List[List[Int]]()      
    var abc = List[Int]()
    while (i < n) {
      val t = triples(i).toList.take(3)      
      /* right triangle
       * a = 2R b = 2R p/q
       * c = 2R ( sqrt(q^2-p^2) / q )
       * SO R = kq with k <= lim/q, i.e., the sum of radii is q*kc*(kc+1)/2
       * with kc = lim/q
       */ 
      val kcRight = lim/t(2)
      s = s + t(2) * kcRight * (kcRight + 1)/2
      /* isosceles triangle
       * a = b = 2R p/q or a = b = 2R r/q
       * c = 2R ( 2p sqrt(q^2-p^2) / q^2 )
       * SO R = kq^2 with k <= lim/q^2, i.e., the sum of radii is 2*q^2*kc*(kc+1)/2
       * with kc = lim/q^2
       * we have twice because there are 2 isosceles triangles possible
       */ 	
      val kcIsosceles = lim/(t(2)*t(2))
      s = s + t(2) * t(2) * kcIsosceles * (kcIsosceles + 1)
      /* the general case:
       * a = 2R p1/q1
       * b = 2R p2/q2
       * c = 2R (p1*sqrt(q2^2 - p2^2) + p2*sqrt(q1^2 - p1^2)) / q1q2
       */
      j = i + 1
      while (j < n) {
	val tp = triples(j).toList.take(3)
	val q1q2 = t(2)*tp(2)
	val mq1q2 = lcm(t(2), tp(2))
	val q2p1 = tp(2)*t(0)
	val q1p2 = t(2)*tp(0) 
	val q2r1 = tp(2)*t(1)
	val q1r2 = t(2)*tp(1) 
	if (q2p1 != q1p2 || q2r1 != q1r2) {	
	  val factor1 = t(0)*tp(1) + t(1)*tp(0)
	  val d1 = gcd(q1q2, factor1)
	  val r1 = q1q2 / d1
	  val l1 = lcm(t(2), lcm(tp(2), r1))
	  val kc1 = lim / l1
	  val ns = l1 * kc1 * (kc1 + 1) / 2
	  val goodK1 = List.range(1, kc1+1)
	  var a = l1*t(0)/t(2)
	  var b = l1*tp(0)/tp(2)
	  var c = l1*factor1/q1q2
	  if (kc1 != 0 && a != b && b != c && c != a) {
	    abc = List(a,b,c).sorted
	    print("abc = " + (a, b, c) + " sabc = " + (a+b+c) + " t = " + t + " tp = " + tp + " l1 = " + l1 + " factor1 = " + factor1 + " tp0 = " + tp(0) + " kc1 = " + kc1 + " d1 = " + d1)
	    if (d1 == 1) {
	      //if (factors(factor1) == 0) factors(factor1) = 1
	      if (factors(c) == 0) factors(c) = 1
	      s = s + ns
	      println("")
	      abcL = abc +: abcL
	    }
	    //else if (factors(tp(0)) == 0) {
	    else if (factors(b) == 0) {
	      println("")
	      s = s + ns
	      abcL = abc +: abcL
	    }
	    else { 
	      println(" not added")
	      nabcL = abc +: nabcL
	    }
	  }
	  a = l1*t(1)/t(2)
	  b = l1*tp(1)/tp(2)
	  if (kc1 != 0 && a != b && b != c && c != a) {
	    abc = List(a,b,c).sorted
	    print("abc = " + (a, b, c) + " sabc = " + (a+b+c) + " t = " + t + " tp = " + tp + " l1 = " + l1 + " factor1 = " + factor1 + " tp1 = " + tp(1) + " kc1 = " + kc1 + " d1 = " + d1)
	    if (d1 == 1) {
	      //if (factors(factor1) == 0) factors(factor1) = 1
	      if (factors(c) == 0) factors(c) = 1
	      println("")
	      s = s + ns
	      abcL = abc +: abcL
	    }
	    //else if (factors(tp(1)) == 0) {
	    else if (factors(b) == 0) {
	      println("")
	      s = s + ns
	      abcL = abc +: abcL
	    }
	    else { 
	      println(" not added")
	      nabcL = abc +: nabcL
	    }
	  }
	}
	if (q2p1 != q1r2 || q2r1 != q1p2) {	
	  val factor2 = t(1)*tp(1) + t(0)*tp(0)
	  val d2 = gcd(q1q2, factor2)
	  val r2 = q1q2 / d2
	  val l2 = lcm(mq1q2, r2)
	  val kc2 = lim / l2
	  val goodK2 = List.range(1,kc2+1)
	  val ns = l2 * kc2 * (kc2 + 1) / 2
	  var a = l2*t(0)/t(2)
	  var b = l2*tp(1)/tp(2)
	  val c = l2*factor2/q1q2
	  if (kc2 != 0 && a != b && b != c && c != a) {
	    abc = List(a,b,c).sorted
	    print("abc = " + (a, b, c) + " sabc = " + (a+b+c) + " t = " + t + " tp = " + tp + " l2 = " + l2 + " factor2 = " + factor2 + " tp1 = " + tp(1) + " kc2 = " + kc2 + " d2 = " + d2)
	    if (d2 == 1) {
	     // if (factors(factor2) == 0) factors(factor2) = 1
	      if (factors(c) == 0) factors(c) = 1
	      println("")
	      s = s + ns	   
	      abcL = abc +: abcL
	    }
	    //else if (factors(tp(1)) == 0) {
	    else if (factors(b) == 0) {
	      println("")
	      s = s + ns
	      abcL = abc +: abcL
	    }
	    else {
	      nabcL = abc +: nabcL
	      println(" not added")
	    }
	  }
	  a = l2*t(1)/t(2) 
	  b = l2*tp(0)/tp(2)
	  if (kc2 != 0 && a != b && b != c && c != a) {
	    abc = List(a,b,c).sorted
	    print("abc = " + (a, b, c) + " sabc = " + (a+b+c) + " t = " + t + " tp = " + tp + " l2 = " + l2 + " factor2 = " + factor2 + " tp0 = " + tp(0) + " kc2 = " + kc2 + " d2 = " + d2)
	    if (d2 == 1) {
	      //if (factors(factor2) == 0) factors(factor2) = 1
	      if (factors(c) == 0) factors(c) = 1
	      println("")
	      s = s + ns
	      abcL = abc +: abcL
	    }
	    //else if (factors(tp(0)) == 0) { 
	    else if (factors(b) == 0) {
	      println("")
	      s = s + ns
	      abcL = abc +: abcL
	    }
	    else {
	      println(" not added")
	      nabcL = abc +: nabcL
	    }
	  }
	}
	j = j + 1
      }
      i = i + 1
    }
    println( "Result: " + s)
    val l = abcL.sortBy(l=>l(0))
    val temp = List.range(0,l.length-1).filter(i => l(i) == l(i+1))
    val repeated = (temp map {i => l(i)}).distinct
    println("abcL.length = " + abcL.length + " nabcL.length = " + nabcL.length +
	    " abcL.toSet.size = " + abcL.toSet.size + " nabcL.toSet.size = " + nabcL.toSet.size +
	    " nabc - abc = " + (nabcL.toSet -- abcL.toSet).toList.sortBy(l=>l(0)) + 
	    " repeated in abcL = " + repeated )
    println( "Time (s): " + ( System.currentTimeMillis() - start ) / 1000. )
  }

def p373Tri(lim: Int) = {
  val start = System.currentTimeMillis
  val file = "/media/ubuntuPart2/docs/topcoder/pythagorean-triples.txt"
  val rf: Input = Resource.fromFile(file)
  val lines = rf.lines().filter(_!="")
  val nlines = lines.size
  println("n lines = " + nlines)
  var s = 0L
  var clines = 0
  val linesSplit = lines.toList map { li => li.trim.split(" ").filter(_!="") map {_.toInt} }
  val linesFilter1 = linesSplit.filter(_.length >= 3)
  val triples = linesFilter1.filter(t => t(2) <= 2*lim)
  val n = triples.length
  println("triples length = " + n)
  var i = 0 
  var j = 1
  var abc = List[List[Double]]()
  val triangles: Array[Array[Array[Long]]] = new Array(2*lim+1, 2*lim+1, 2*lim+1)
  while (i < n) {
    val t = triples(i).toList.take(3)      
    /* right triangle
     * a = 2R b = 2R p/q
     * c = 2R ( sqrt(q^2-p^2) / q )
     * SO R = kq with k <= lim/q, i.e., the sum of radii is q*kc*(kc+1)/2
     * with kc = lim/q
     */ 
    val kcRight = lim/t(2)
    s = s + t(2) * kcRight * (kcRight + 1)/2
    /* isosceles triangle
     * a = b = 2R p/q or a = b = 2R r/q
     * c = 2R ( 2p sqrt(q^2-p^2) / q^2 )
     * SO R = kq^2 with k <= lim/q^2, i.e., the sum of radii is 2*q^2*kc*(kc+1)/2
     * with kc = lim/q^2
     * we have twice because there are 2 isosceles triangles possible
     */ 	
    val kcIsosceles = lim/(t(2)*t(2))
    s = s + t(2) * t(2) * kcIsosceles * (kcIsosceles + 1)
    /* the general case:
     * a = 2R p1/q1
     * b = 2R p2/q2
     * c = 2R (p1*sqrt(q2^2 - p2^2) + p2*sqrt(q1^2 - p1^2)) / q1q2
     */
    j = i + 1
    while (j < n) {
      val tp = triples(j).toList.take(3)
      val q1q2 = t(2)*tp(2)
      val mq1q2 = lcm(t(2), tp(2))
      val q2p1 = tp(2)*t(0)
      val q1p2 = t(2)*tp(0) 
      val q2r1 = tp(2)*t(1)
      val q1r2 = t(2)*tp(1) 
      if (q2p1 != q1p2 || q2r1 != q1r2) {	
	val factor1 = t(0)*tp(1) + t(1)*tp(0)
	val d1 = gcd(q1q2, factor1)
	val r1 = q1q2 / d1
	val l1 = lcm(t(2), lcm(tp(2), r1))
	val kc1 = lim / l1
	val ns = l1 * kc1 * (kc1 + 1) / 2
	val goodK1 = List.range(1, kc1+1)
	//if (!(q2p1 == q1p2 || q2p1 == factor1 || q1p2 == factor1) ) {     
	//if (l1*t(0)/t(2) != l1*tp(0)/tp(2) && l1*tp(0)/tp(2) != l1*factor1/q1q2 && l1*factor1/q1q2 != l1*t(0)/t(2) ) {
	var a = l1*t(0)/t(2)
	var b = l1*tp(0)/tp(2)
	var c = l1*factor1/q1q2
	if (kc1 != 0 && a != b && b != c && c != a) {
	  val l = List(a,b,c).sorted
	  if (triangles(l(0))(l(1))(l(2)) == 0) { 
	    triangles(l(0))(l(1))(l(2)) = 1
	    //println("abc = " + (a,b,c) + " t = " + t + " tp = " + tp + " l1 = " + l1 + " factor1 = " + factor1 + " kc1 = " + kc1 + " d1 = " + d1)
	    s = s + ns
	    //abc = abc ::: (goodK1 map {k => List(2.*k*l1*t(0)/t(2), 2.*k*l1*tp(0)/tp(2), 2.*k*l1*factor1/q1q2)})
	  }
	}
	//if (q2r1 != q1r2  && q2p1 != factor1 && q1p2 != factor1) {
	//if (l1*t(1)/t(2) != l1*tp(1)/tp(2) && l1*factor1/q1q2 !=  l1*tp(1)/tp(2) &&  l1*factor1 != l1*t(1)/t(2) ) {
	a = l1*t(1)/t(2)
	b = l1*tp(1)/tp(2)
	if (kc1 != 0 && a != b && b != c && c != a) {
	  val l = List(a,b,c).sorted
	  if (triangles(l(0))(l(1))(l(2)) == 0) { 
	    triangles(l(0))(l(1))(l(2)) = 1
	    //println("abc = " + (a,b,c) + " t = " + t + " tp = " + tp + " l1 = " + l1 + " factor1 = " + factor1 + " kc1 = " + kc1 + " d1 = " + d1)
	    s = s + ns
	    //abc = abc ::: (goodK1 map {k => List(2.*k*l1*t(1)/t(2), 2.*k*l1*tp(1)/tp(2), 2*k*l1*factor1/q1q2)})
	  }
	}
      }
      if (q2p1 != q1r2 || q2r1 != q1p2) {	
	val factor2 = t(1)*tp(1) + t(0)*tp(0)
	val d2 = gcd(q1q2, factor2)
	val r2 = q1q2 / d2
	val l2 = lcm(mq1q2, r2)
	val kc2 = lim / l2
	val goodK2 = List.range(1,kc2+1)
	val ns = l2 * kc2 * (kc2 + 1) / 2
	//if (q2p1 != q1r2 && q2p1 != factor2 && q1r2 != factor2) {
	//if (l2*t(0)/t(2) != l2*tp(1)/tp(2) && l2*tp(1)/tp(2) != l2*factor2/q1q2 &&  l2*factor2/q1q2 != l2*t(0)/t(2) ) {
	var a = l2*t(0)/t(2)
	var b = l2*tp(1)/tp(2)
	val c = l2*factor2/q1q2
	if (kc2 != 0 && a != b && b != c && c != a) {
	  val l = List(a,b,c).sorted
	  if (triangles(l(0))(l(1))(l(2)) == 0) { 
	    triangles(l(0))(l(1))(l(2)) = 1
	    //println("abc = " + (a,b,c) + " t = " + t + " tp = " + tp + " l2 = " + l2 + " factor2 = " + factor2 + " kc2 = " + kc2 + " d2 = " + d2)
	    s = s + ns
	    //abc = abc ::: (goodK2 map {k => List(2.*k*l2*t(0)/t(2), 2.*k*l2*tp(1)/tp(2), 2.*k*l2*factor2/q1q2)})
	  }
	}
	//if (q2r1 != q1p2 && q2r1 != factor2 && q1p2 != factor2) {
	a = l2*t(1)/t(2) 
	b = l2*tp(0)/tp(2)
	if (kc2 != 0 && a != b && b != c && c != a) {
	  val l = List(a,b,c).sorted
	  if (triangles(l(0))(l(1))(l(2)) == 0) { 
	    triangles(l(0))(l(1))(l(2)) = 1
	    //println("abc = " + (a,b,c) + " t = " + t + " tp = " + tp + " l2 = " + l2 + " factor2 = " + factor2 + " kc2 = " + kc2 + " d2 = " + d2)
	    s = s + ns
	    //abc = abc ::: (goodK2 map {k => List(2.*k*l2*t(1)/t(2), 2.*k*l2*tp(0)/tp(2), 2.*k*l2*factor2/q1q2)})
	  }
	}
      }      
      j = j + 1
    }
  i = i + 1
  }

  println( "Result: " + s)
  println( "Time (s): " + ( System.currentTimeMillis() - start ) / 1000. )
  //abc
}

def p373Naive(lim: Int) = {
  val start = System.currentTimeMillis
  val file = "/media/ubuntuPart2/docs/topcoder/pythagorean-triples.txt"
  val rf: Input = Resource.fromFile(file)
  val lines = rf.lines().filter(_!="")
  val nlines = lines.size
  println("n lines = " + nlines)
  var s = 0L
  var clines = 0
  val linesSplit = lines.toList map { li => li.trim.split(" ").filter(_!="") map {_.toInt} }
  val linesFilter1 = linesSplit.filter(_.length >= 3)
  val triples = linesFilter1.filter(t => t(2) <= 2*lim)
  val n = triples.length
  println("triples length = " + n)
  var mR = Map[Int, Set[List[Int]]]().empty
  List.range(5, lim+1) foreach {i => mR = mR.updated(i, Set[List[Int]]())}
  var i = 0 
  var j = 1
  var radius = 5
  while (radius <= lim) {
    i = 0
    while (i < n) {
      val t = triples(i).toList.take(3)      
      //if (i == 2) println("radius = " + radius + " t = " + t.toList)
      if ((radius % t(2)) == 0) { 
      /* right triangle
       * a = 2R b = 2R p/q
       * c = 2R ( sqrt(q^2-p^2) / q )
       */ 
	val a = (radius/t(2))*2*t(1)
	val c = (radius/t(2))*2*t(0)
	val b = 2*radius
	mR = mR.updated(radius, mR(radius) + List(a,b,c).sorted)
	println("added " + (a,b,c))
      /* check if possible isosceles triangle
       * a = b = 2R p/q
       * c = 2R ( 2p sqrt(q^2-p^2) / q^2 )
       */ 	
	if (((radius*t(0)*t(1)) % (t(2)*t(2))) == 0 ) {
	  val a1 = (radius/t(2))*2*t(1)
	  val a2 = (radius/t(2))*2*t(0)
	  val c = (radius/(t(2)*t(2)))*4*t(1)*t(0)
	  mR = mR.updated(radius, mR(radius) + List(a1,a1,c).sorted + List(a2,a2,c).sorted)
	  println("added isosceles " + (a1,a1,c) + " and " + (a2,a2,c))
	}
	/* the general case:
	 * a = 2R p1/q1
	 * b = 2R p2/q2
	 * c = 2R (p1*sqrt(q2^2 - p2^2) + p2*sqrt(q1^2 - p1^2)) / q1q2
	 */
	j = i + 1
	while (j < n) {
	  val tp = triples(j).toList.take(3)
	  if ( (radius % tp(2)) == 0 ) {
	    val q1q2 = t(2)*tp(2)
	    val a1 = (radius/t(2))*2*t(0)
	    val b1 = (radius/tp(2))*2*tp(0)
	    val a2 = (radius/t(2))*2*t(1)
	    val b2 = (radius/tp(2))*2*tp(1)
	    val factor1 = t(0)*tp(1) + t(1)*tp(0)
	    val cond1 = (radius*factor1) % q1q2 == 0
	    if (cond1) {
	      //careful: radius is not necessarily multiple of q1q2 as it may just as well be that factor1 contains some factors!!
	      //val c = (radius/q1q2)*2*factor1
	      val c = 2*radius*factor1/q1q2
	      mR = mR.updated(radius, mR(radius) + List(a1,b1,c).sorted + List(a2,b2,c).sorted )
	      println("added " + (a1,b1,c) + " and " + (a2,b2,c)+ " at t = " + t + " tp = " + tp + " (i,j) = " + (i, j) + " radius = " + radius)
	    }
	    val factor2 = t(1)*tp(1) + t(0)*tp(0)
	    val cond2 = (radius*factor2) % q1q2 == 0
	    if (cond2) {
	      val c = 2*radius*factor2/q1q2
    	      mR = mR.updated(radius, mR(radius) + List(a1,b2,c).sorted + List(a2,b1,c).sorted )
	      println("added " + (a1,b2,c) + " and " + (a2,b1,c) + " at t = " + t + " tp = " + tp + " (i,j) = " + (i, j) + " radius = " + radius)
	    }
	  }
	  j = j + 1
	}
      }
      i = i + 1
    }
    radius = radius + 1
  }

  val mRF = mR.filter(_._2.size!=0)
  s = mRF.foldLeft(0)((r, c) => ( r + c._1*c._2.size))
  println( "Result: " + mRF.takeRight(20).foldLeft("")(_+"\n"+_) + "\n s = " + s)
  println( "Time (s): " + ( System.currentTimeMillis() - start ) / 1000. )
  mRF
}

}
