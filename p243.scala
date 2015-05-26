/* A positive fraction whose numerator is less than its denominator is
 * called a proper fraction.
 * For any denominator, d, there will be d1
proper fractions; for example, with d = 12: 1/12 , 2/12 , 3/12 , 4/12
, 5/12 , 6/12 , 7/12 , 8/12 , 9/12 , 10/12 , 11/12 .

   We shall call a fraction that cannot be cancelled down a resilient
fraction.  Furthermore we shall define the resilience of a
denominator, R(d), to be the ratio of its proper fractions that are
resilient; for example, R(12) = 4/11 .  In fact, d = 12 is the
smallest denominator having a resilience R(d) < 4/10 .

   Find the smallest denominator d, having a resilience R(d) <
15499/94744 .  */ 

object P243 {

  def gcd(a: Long, b: Long): Long = if(b == 0) a else gcd(b, a % b)


    
//slow with gcd
//  def isResilient(a: Long, b: Long): Boolean = if (gcd(a,b) == 1) true else false
  def nRes(d: Int) = {
    // isRes(i) == 1 means that gcd(i, d) == 1 
    val isRes = Array.fill(d)(1)
    List.range(2, d/2) foreach {
      i => 
	if (d % i == 0) {
	  isRes(i) = 0
	  List.range(i, d/i) foreach { k => isRes(i*k) = 0 }
	}
	else List.range(i, d/i) foreach { k => if(isRes(k) == 0) isRes(i*k) = 0 }
    }
    //delete 1 for 0 
    isRes.filter(_==1).length - 1
  }

  def main(args: Array[String]) {

    var f1 = args(0).toInt
    var f2 = args(1).toInt
    val v = f1*1./f2
    var d = 12

    val start = System.currentTimeMillis
    var stop = false
    while(!stop) { 
      val nres = nRes(d)
      println((d,nres))
      if (nres*1./d < v) stop = true
      //if (d > 32) stop = true 
      else d += 1
    }
    
    println(" d = " + (d - 1))

    //println("r(" + d + ") = " + nRes(d))
    println( "Time (s): " + ( System.currentTimeMillis() - start ) / 1000. )
  }
}

/* slow also with this adapt of sieve because i check for \phi(n) for each n...
 * while we know \phi(n) is multiplicative....
 * remember that the number of numbers coprime with n is euler totient function,
 * \phi(n) which is equal to n*(1-1/p1)*...*(1-1/pr)
 * for n = p1^k1*...pr^kr
 * R(n) = \phi(n)/(n-1) = n/(n-1)*(1-1/p1)*...*(1-1/pr)
 * so (1-1/p1)*...*(1-1/pr) must be smaller than v = 15499/94744
 * since we look for the smallest n, we check consecutive primes
 * manually:
 *
scala> val n = p.take(9).foldLeft(1)(_*_)

scala> val p = List(2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 51, 53, 57)

scala> val r = p.take(9).foldLeft(1.)((r,c)=>(r*(1-1./c)))
r: Double = 0.16358819535559344

scala> v
res24: Double = 0.1635881955585578

scala> val g = r*n/(n-1)
g: Double = 0.1635881960888674 //so this is bigger than v, so i increase the smallest prime, i.e., 2^2

scala> val g = r*n*2/(2*n-1)
g: Double = 0.1635881957222304

scala> g > v
res25: Boolean = true

scala> val g = r*n*4/(4*n-1)
g: Double = 0.1635881955389119

scala> g > v
res26: Boolean = false

scala> 4*n
res27: Int = 892371480

*/ 
 
