object P373 {

import scalax.io._
import scalax.file.{ FileOps, Path, NotFileException }

  def main(args: Array[String]) {
    p373T2(args(0).toInt)
  }

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

  def gcdL(a: Long, b: Long): Long = if(b == 0) a else gcdL(b, a % b)

  def lcmL(a: Long, b: Long) = a * (b / gcdL(a, b))

  def p373T2(lim: Int) = {
    val start = System.currentTimeMillis
    val file = "/media/ubuntuPart2/docs/topcoder/pythagorean-triples.txt"
    val rf: Input = Resource.fromFile(file)
    val lines = rf.lines().filter(_!="")
    val nlines = lines.size
    println("n lines = " + nlines)
    var s = 0.
    var clines = 0
    val linesSplit = lines.toList map { li => li.trim.split(" ").filter(_!="") map {_.toInt} }
    val linesFilter1 = linesSplit.filter(_.length >= 3)
    val triples = linesFilter1.filter(t => t(2) <= 2*lim)
    val n = triples.length
    println("triples length = " + n)
    var i = 0 
    var j = 1
    val visited: Array[Array[Set[(Long,Long,Long)]]] = new Array(n,n)
    while (i < n) {
      val t = triples(i).toList.take(3)      
      /* right triangle
       * a = 2R b = 2R p/q
       * c = 2R ( sqrt(q^2-p^2) / q )
       * SO R = kq with k <= lim/q, i.e., the sum of radii is q*kc*(kc+1)/2
       * with kc = lim/q
       */ 
      val kcRight = lim/t(2)
      s = s + 1. * t(2) * kcRight * (kcRight + 1)/2
      if (s < Double.MinValue || s > Double.MaxValue) throw new ArithmeticException("Double out of bounds")
      //println("kcRight = " + kcRight + " s = " + s)
      /* isosceles triangle
       * a = b = 2R p/q or a = b = 2R r/q
       * c = 2R ( 2p sqrt(q^2-p^2) / q^2 )
       * SO R = kq^2 with k <= lim/q^2, i.e., the sum of radii is 2*q^2*kc*(kc+1)/2
       * with kc = lim/q^2
       * we have twice because there are 2 isosceles triangles possible
       */ 	
      val kcIsosceles = lim/(t(2)*t(2))
      s = s + 1. * t(2) * t(2) * kcIsosceles * (kcIsosceles + 1)
      if (s < Double.MinValue || s > Double.MaxValue) throw new ArithmeticException("Double out of bounds")
      //println("kcIso = " + kcIsosceles+ " s = " + s)
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
	val q1q2 = 1L*t(2)*tp(2)
	val mq1q2 = lcmL(t(2), tp(2))
	/*careful, you might need to work with LONG for 10^7*/
	val p1q2 = 1L*t(0)*tp(2)
	val q1p2 = 1L*t(2)*tp(0) 
	val r1q2 = 1L*t(1)*tp(2)
	val q1r2 = 1L*t(2)*tp(1) 
	val p1p2 = 1L*t(0)*tp(0)
	val p1r2 = 1L*t(0)*tp(1)
	val r1r2 = 1L*t(1)*tp(1)
	val r1p2 = 1L*t(1)*tp(0)
	//val maxt2 = math.max(t(2),tp(2))
	//val mint2 = math.min(t(2),tp(2))
	/*CASE I.*/
	val factor1 = p1r2 + r1p2 
	val factor2 = r1r2 + p1p2
	val d1 = gcdL(q1q2, factor1)
	val d2 = gcdL(q1q2, factor2)
	//val auxqr1 =  q1q2 / d1
	val qr1 = q1q2 / d1
	//val auxfr1 = factor1 / d1
	//val fr1 = if (auxfr1 > scala.Int.MaxValue) lim + 1 else auxfr1.toInt
	val fr1 = factor1 / d1
	//it's ok to work with INT because if the result of lcm is a LONG then anyhow lim < l1 !!!! (and we need to consider only those lcm which are <= lim)
        val l1 = lcmL(t(2), lcmL(tp(2), qr1)) 
	//val l1 = if (l1L > lim) lim + 1 else l1L.toInt
	val kc1 = lim / l1
        val ns1 = 1. * l1 * kc1 * (kc1 + 1) / 2
	val qr2 = q1q2 / d2
	val fr2 = factor2 / d2
	val l2 = lcmL(t(2), lcmL(tp(2), qr2)) 
	//val l2 = lcmL(mq1q2, qr2)
	//val l2 = if (l2L > lim) lim + 1 else l2L.toInt
	val kc2 = lim / l2
	val ns2 = 1. * l2 * kc2 * (kc2 + 1) / 2
	if (l1 < 0 || l2 < 0 || d1 < 0 || d2 < 0 || fr1 < 0 || fr2 < 0 || ns1 < 0 || ns2 < 0) 
	  println("ERRRRRRRRRRRRRRRRRRROR: (i,j) = " + (i,j) + "(t, tp) = " + (t, tp) + " l1 = " + l1 + " d1 = " + d1 + " qr1 = " + qr1 + 
		" l2 = " + l2 + " d2 = " + d2 + " qr2 = " + qr2)

/*	if (d1 < 0 || d2 < 0) 
	  println("ERRRRRRRRRRRRRRRRRRROR: (i,j) = " + (i,j) + "(t, tp) = " + (t, tp) + " d1 = " + d1 + " d2 = " + d2 + " q1q2 = " + q1q2 + " factor1 = " + factor1 + " factor2 = " + factor2)
	  */
	val a1 = 1.*p1/q1
	val a2 = 1.*r1/q1
	val b1 = 1.*p2/q2
	val b2 = 1.*r2/q2
	val c1 = 1.*fr1/qr1
	val c2 = 1.*fr2/qr2
	if(kc1 > 0 || kc2 > 0) {	  
	  if (kc1 > 0 && visited(i)(j) == null) visited(i)(j) = Set((fr1,qr1,l1))
	  if (kc2 > 0 && visited(i)(j) == null) visited(i)(j) = Set((fr2,qr2,l2))
	  if (kc1 > 0 && visited(i)(j) != null && !visited(i)(j)((fr1,qr1,l1))) visited(i)(j) = visited(i)(j) + ((fr1,qr1,l1))
	  if (kc2 > 0 && visited(i)(j) != null && !visited(i)(j)((fr2,qr2,l2))) visited(i)(j) = visited(i)(j) + ((fr2,qr2,l2)) 
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
	      if (notIsoCI1) { 
		s = s + ns1
		if (s < Double.MinValue || s > Double.MaxValue) throw new ArithmeticException("Double out of bounds")
	      }
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
		if ( d1 > 1 && kc1 > 0 && notIsoCI1 && ( (visited(k)(j) != null && visited(k)(j)((p1,q1,l1))) || (visited(i)(k) != null && visited(i)(k)((p2,q2,l1))) ) ) repeatedI1 = true
		if ( d1 > 1 && kc1 > 0 && notIsoCI2 && ( (visited(k)(j) != null && visited(k)(j)((r1,q1,l1))) || (visited(i)(k) != null && visited(i)(k)((r2,q2,l1))) ) ) repeatedI2 = true
		if ( d2 > 1 && kc2 > 0 && notIsoCII1 && ( (visited(k)(j) != null && visited(k)(j)((p1,q1,l2))) || (visited(i)(k) != null && visited(i)(k)((r2,q2,l2))) ) ) repeatedII1 = true
		if ( d2 > 1 && kc2 > 0 && notIsoCII2 && ( (visited(k)(j) != null && visited(k)(j)((r1,q1,l2))) || (visited(i)(k) != null && visited(i)(k)((p2,q2,l2))) ) ) repeatedII2 = true
		k = k + 1
	      }
	      if (k == i) {		
		if (k == 0) k = 1
		//println("at (i,j) = " + (i,j) + " reached k = " + k + " visited = " + (visited map {_.toList}).toList.take(2))
		while (k < j && (repeatedI1 == false || repeatedI2 == false || repeatedII1 == false || repeatedII2 == false)) {
		  if (visited(i)(k) != null) {
		    if (d1 > 1 && kc1 > 0 && !repeatedI1 && visited(i)(k)((p2,q2,l1)) ) repeatedI1 = true
		    if (d1 > 1 && kc1 > 0 && !repeatedI2 && visited(i)(k)((r2,q2,l1)) ) repeatedI2 = true
		    if (d2 > 1 && kc2 > 0 && !repeatedII1 && visited(i)(k)((r2,q2,l2)) ) repeatedII1 = true
		    if (d2 > 1 && kc2 > 0 && !repeatedII2 && visited(i)(k)((p2,q2,l2)) ) repeatedII2 = true
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
    s
  }
}
