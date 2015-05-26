object P248 { 

 def ldf(k : Long, n : Long) : Long = {
   if (n % k == 0) k
   else if ((k*k) > n) n
   else ldf((k+1), n)
 }

def ld(n : Long) : Long = {
   ldf(2, n)
 }

 def factors(n : Long) : List[Long] = n match {
   case 1 => Nil;
   case _ => {
     val p = ld(n)
     p :: factors(n / p)
   }
 }

  import scalax.io._
  import scalax.file.{ FileOps, Path, NotFileException }

 def tests() = {
  val file = "/media/ubuntuPart2/docs/topcoder/outP248.txt"
  val rf: Input = Resource.fromFile(file)
  val lines = rf.lines().filter(_!="")
  val nlines = lines.size
  println("n lines = " + nlines)
  var maxFactors = 1
  var nelems = 0
  var maxV = 0L
  var factorsMap = Map[Long,List[Long]]().empty
  lines foreach {
    l => 
      val elems = l.trim.split(" ").filter(_!="").toList.map{_.toLong}
      elems foreach {
	e => 
	  factorsMap = factorsMap + (e -> factors(e))
      }
      nelems = nelems + elems.length
  }
 println(" total nb of elems = " + nelems)
 factorsMap
 }


 val divs2 =  divs.takeWhile(_<math.pow(lim,1./2).toInt+1)
 val cand2 = divs2.filter(d => isPrime(d+1))

 //decomp lim = 13! in 3 prime factors: 2 possib: f1*f2*f3, f1^2*f3 
 val divs3 =  divs.takeWhile(_<math.pow(lim,1./3).toInt+1)
 val cand3 = divs3.filter(d => isPrime(d+1))
 val prod3 = for(f1 <- cand3; f2 <- cand2.takeWhile(_<math.sqrt(lim/f1)).dropWhile(_<f1);val f12 = f1*f2; val f3 = lim/f12; if ((lim % f12 == 0) && isPrime((f3 + 1).toInt))) yield (1L*(f1+1)*(f2+1)*(f3+1))
 val prod3S = for(f1 <- cand3; val t = (lim % (f1*(f1+1))); val f3 = lim/(f1*(f1+1)); if (t == 0 && isPrime((f3 + 1).toInt))) yield (1L*(f1+1)*(f1+1)*(f3+1))
 val prod3f = (prod3 ::: prod3S).distinct.sorted


 //decomp lim = 13! in 4 prime factors: ? possib: f1*f2*f3*f4, f1^2*f2*f3, f1^2*f2^2
 val divs4 =  divs.takeWhile(_<math.pow(lim,1./4).toInt+1)
 val cand4 = divs4.filter(d => isPrime(d+1))
 val prod3 = for(f1 <- cand4; f2 <- cand3.takeWhile(_<math.sqrt(lim/f1)).dropWhile(_<f1); val f123 = f1*f2*f3; val f4 = lim/f123; if ((lim % f123 == 0) && isPrime((f4 + 1).toInt))) yield (1L*(f1+1)*(f2+1)*(f3+1)*(f4+1))
 val prod3S = for(f1 <- cand3; val t = (lim % (f1*(f1+1))); val f3 = lim/(f1*(f1+1)); if (t == 0 && isPrime((f3 + 1).toInt))) yield (1L*(f1+1)*(f1+1)*(f3+1))
 val prod3f = (prod3 ::: prod3S).distinct.sorted


 def main(args : Array[String]) {
   val start = System.currentTimeMillis
   val map = tests()
   println( "Time (s): " + ( System.currentTimeMillis() - start ) / 1. )
 }
}
