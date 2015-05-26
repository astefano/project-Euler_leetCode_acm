object P60 {

 import scalax.io._
 import scalax.file.{ FileOps, Path, NotFileException }

def isPrime(n: Long) = n > 1 && !List.range(2, math.floor(math.sqrt(n)).toInt+1).exists(i => (n%i)==0)

def main(args: Array[String]) {

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
/*
  val file2 = "/media/ubuntuPart2/docs/topcoder/outp60.txt"
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
	  (s + 2*p < 76721)
	}      
  }
*/

  primes = 3L+:primes.drop(3)
  println(primes.take(10))

  var i1 = 0
  var i2 = 0
  var i3 = 0
  var i4 = 0
  var i5 = 0
  val min = 34427
  var p1 = primes(i1)
  while(p1 < min/5) {
    val p1 = primes(i1)
    i2 = i1 + 1
    var p2 = primes(i2)
    while (p2 < (min-p1)/4) {
      p2 = primes(i2)
      i3 = i2 + 1
      var p3 = primes(i3)
      while (p3 < (min-p1-p2)/3) {
	p3 = primes(i3)
	i4 = i3 + 1
	//while (i4 < 300 && !found) {
	  //val p4 = primes(i4)
	  //i5 = i4 + 1
	  //while (i5 < 330 && !found) {
	   // val p5 = primes(i5)
	    if (!List(p1,p2,p3).combinations(2).toList.map{x => List(x(0).toString+x(1).toString, x(1).toString+x(0).toString)}.flatten.exists(x => !(isPrime(x.toLong)))) { 
		    //found = true
		    println(p1 + " " + p2 + " " + p3) 
	    //}
	    //i5 += 1
	  //}
	  //i4 += 1
	}
	i3 += 1
      }
      i2 += 1
    }
    i1 += 1
  }
  
  }
}
