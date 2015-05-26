def test(p: Int, pL: Set[Int]) = {
  val perms0 = p.toString.permutations.toList map {_.toInt}
  val perms = perms0.toSet intersect pL
  val r = for(p1 <- perms; p2 <- (perms - p1); p3<- (perms - p1 - p2); if (math.abs(p1-p2) == math.abs(p2-p3))) yield (p1, p2, p3)
  if (r.size > 0) {
    println(r.toList(0))
    r.toList(0).toString 
  }
  else ""
}

import scalax.io._
import scalax.file.{ FileOps, Path, NotFileException }

def find() = {
  val start = System.currentTimeMillis
  val file = "/media/ubuntuPart2/docs/topcoder/primes4Digits.txt"
  val rf: Input = Resource.fromFile(file)

  val lines = rf.lines().filter(_!="")
  val nlines = lines.size
  println("n lines = " + nlines)
  var i = 0
  
  val primes = lines.toList.map{
    l => 
      l.trim.split(" ").filter(_!="").map(_.toInt).toList
  }.flatten

  println(primes)

  //primes.exists(cp => (cp != 1487 && test(cp, primes.toSet) != ""))
  primes.foreach{test(_, primes.toSet)}

  println( "Time (s): " + ( System.currentTimeMillis() - start ) / 1000. )
}
