object P231 {

//impl from P. Goetgheluck, Computing Binomial Coefficients

def getExp(p: Int, n0: Int, k0: Int): Int = {
  var e = 0
  var r = 0
  var n = n0
  var k = k0
  var a = 0 
  var b = 0 
  if (k > n/2) k = n - k 
  if (p > n - k) {
    //println("1:p = " + p + " e = 1")
    return 1
  }
  if (n - k >= p && p > n/2) {
    //println("2:p = " + p + " e = 0")
    return 0
  }
  if (p*p > n) if ((n % p) < (k % p)) {
    //println("3:p = " + p + " e = 1")    
    return 1
    } else {
      return 0
    }
  while ( n > 0 ) { 
    a = n % p
    b = (k % p) + r
    n = n / p
    k = k / p
    //println(a + " " + b + " " + n + " " + k)
    if (a < b) { e += 1; r = 1} else r = 0
  }
  //println("p = " + p + " e = " + e)
  return e
}

  //val n = 20000000
  //val k = 15000000
  
  import scalax.io._
  import scalax.file.{ FileOps, Path, NotFileException }

  def main(args: Array[String]) {

    val n = args(0).toInt

    val k = args(1).toInt

    val start = System.currentTimeMillis       

    val file = "/media/ubuntuPart2/docs/topcoder/primesSmallerThan20millions.txt"
    val rf: Input = Resource.fromFile(file)

    val lines = rf.lines().filter(_!="")
    val nlines = lines.size
    println("n lines = " + nlines)

    var s = BigInt(0)
    lines foreach {
      l => 
	//val primes = l.split("""\\s+""").map{_.trim}.filter{_!=""}.map{_.toInt}
	val res = l.split(' ')
	//println("res = " + res.toList)
	val primes = res.map{_.trim}.filter{_!=""}.map{_.toInt}
	primes foreach {
	  p => 
	    if (p < n) {
	      val e = getExp(p, n, k)
	      s += p*e
	    }
	}
    }
    
    println("res=" + s)

    println( "Time (s): " + ( System.currentTimeMillis() - start ) / 1000. )
  }
} 
