object P60 {
 
 import scalax.io._
 import scalax.file.{ FileOps, Path, NotFileException }

  def isPrime(n: Long) = n > 1 && !List.range(2, math.floor(math.sqrt(n)).toInt+1).exists(i => (n%i)==0)

  def main(args: Array[String]) {
    val min = 34427
    var primes = List[Long]()
    val file = "/media/ubuntuPart2/docs/topcoder/primesSmallerThan10to6.txt"
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

    val file2 = "/media/ubuntuPart2/docs/topcoder/outp60-3temp.txt"
    val rf2: Input = Resource.fromFile(file2)
    val lines2 = rf2.lines().filter(_!="")
    lines2 foreach {
      l => 
	val t = l.trim.split(" ").filter(_!="").toList
      var s = t.foldLeft(0)(_+_.toInt) 
      primes.dropWhile(_<l(2).toInt).takeWhile{
	p => 
	  val v0p = (t(0) + p.toString).toLong
	val vp0 = (p.toString + t(0)).toLong
	val v1p = (t(1) + p.toString).toLong
	val vp1 = (p.toString + t(1)).toLong
	val v2p = (t(2) + p.toString).toLong
	val vp2 = (p.toString + t(2)).toLong
	if (isPrime(v0p) && isPrime(vp0) && isPrime(v1p) && isPrime(vp1) && isPrime(v2p) && isPrime(vp2)) println(t(0) + " " + t(1) + " " + t(2) + " " + p)
	(s + 2*p < min)
      }      
    }
  }
}
