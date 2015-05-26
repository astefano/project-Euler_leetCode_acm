object P60 {

 import scalax.io._
 import scalax.file.{ FileOps, Path, NotFileException }

  def isPrime(n: Long) = n > 1 && !List.range(2, math.floor(math.sqrt(n)).toInt+1).exists(i => (n%i)==0)

  def main(args: Array[String]) {

    var primes = List[Long]()
    val file = "/media/ubuntuPart2/docs/topcoder/primesForP60.txt"
    val rf: Input = Resource.fromFile(file)
    val lines = rf.lines().filter(_!="")
    lines foreach {
      l => 
	val pr = l.trim.split(" ").filter(_!="").toList.map{_.toLong}
      pr foreach {
	p => 
	  primes = primes :+ p
      }
    }

    println(primes.take(20))

    val file2 = "/media/ubuntuPart2/docs/topcoder/outp60-4ff.txt"
    val rf2: Input = Resource.fromFile(file2)
    val lines2 = rf2.lines().filter(_!="")
    var min = 76721L
    lines2 foreach {
      l => 
	val t = l.trim.split(" ").filter(_!="").toList
	val cand = primes.dropWhile(_<t(3).toInt) 
	cand foreach {
	p => 
	  val s = p + t.foldLeft(0)(_+_.toInt)
	  if (s < min) {
	    val v0p = (t(0) + p.toString).toLong
	    val vp0 = (p.toString + t(0)).toLong
	    val v1p = (t(1) + p.toString).toLong
	    val vp1 = (p.toString + t(1)).toLong
	    val v2p = (t(2) + p.toString).toLong
	    val vp2 = (p.toString + t(2)).toLong
	    val v3p = (t(3) + p.toString).toLong
	    val vp3 = (p.toString + t(3)).toLong
	    //if (t(3).toInt > 5900) 
	      //println(t + " " + cand.take(10))
	    if (isPrime(v0p) && isPrime(vp0) && isPrime(v1p) && isPrime(vp1) && isPrime(v2p) && isPrime(vp2) && isPrime(v3p) && isPrime(vp3)) {
	      min = s
	      println(t(0) + " " + t(1) + " " + t(2) + " " + t(3) + " " + p + " " + s)
	    }
	  }
      }
    }
  }

}

/*
3 37 67 5923 194119 200149
733 883 991 18493 55621 76721
*/
