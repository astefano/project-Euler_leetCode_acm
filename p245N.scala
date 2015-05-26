object PrimeFactorisationTest {

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
  val file = "/media/ubuntuPart2/docs/topcoder/p245.txt"
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

 def loadPrimes() = {
// val file = "/media/ubuntuPart2/docs/topcoder/primesSmallerThan10to6.txt"
  val file = "/media/ubuntuPart2/docs/topcoder/primesfor245.txt"
  val rf: Input = Resource.fromFile(file)
  val lines = rf.lines().filter(_!="")
  val nlines = lines.size
  //println("n lines = " + nlines)
  var nprimes = 0
  var primesL = List[Int]()
  lines foreach {
    l => 
      val primes = l.trim.split(" ").filter(_!="").toList.map{_.toInt}
      primesL = primesL ::: primes
      nprimes = nprimes + primes.length
  }
 println("found = " + nprimes)
 primesL
 }

 def testCoresilience(phi: Long, n: Long) = (n - 1) % (n - phi) == 0

 def loadPQ2() = {
   val file = "/media/ubuntuPart2/docs/topcoder/inP245.txt"
   val rf: Input = Resource.fromFile(file)
   val lines = rf.lines().filter(_!="")
   val nlines = lines.size
   println("n lines = " + nlines)
   var eulerphi = Map[Long,Long]().empty
   var c = 0
   lines foreach {
     l => 
       val res = l.trim.split("p =")(1).split(" q =")(1).split("pq =")(1).split(" phi = ")
//       val res = l.trim.split(" ")
       val pq = res(0).trim.toLong
       val phi = res(1).trim.toLong
       eulerphi = eulerphi + (pq -> phi)
       //println("pq = " + pq + " phi = " + phi)
       c += 1
   }
   println("found = " + c)
   eulerphi
 }

 def loadCoResPQ2() = {
   val file = "/media/ubuntuPart2/docs/topcoder/outGP245N2.txt"
   val rf: Input = Resource.fromFile(file)
   val lines = rf.lines().filter(_!="")
   val nlines = lines.size
   println("n lines = " + nlines)
   //var m = Map[Long,List[Long]]().empty
   var m = Map[Long,Long]().empty
   var c = 0
   lines foreach {
     l => 
       val res = l.trim.split(" ")
       val pq = res(0).trim.toLong
       val p = res(1).trim.toLong
       val q = res(2).trim.toLong
       m = m + (pq -> (pq - p - q + 1L))
       c += 1
   }
   println("found = " + c)
   m
 }

 def main(args : Array[String]) {
   val start = System.currentTimeMillis
   val primes = loadPrimes
   val pl = primes.length
   println("nb of primes = " + pl + " last prime = " + primes.last)
   val indices = List.range(0, pl-1)
   val bound = 2*math.pow(10, 11).toLong
   //var eulerphi = Array.ofDim[Long](lim)
   // fill in eulerphi(p*q) for p,q primes; eulerphi(p*q) = eulerphi(p)*eulerphi(q) = (p -1)*(q-1) = pq - p - q + 1
   //var eulerphi2 = Map[Long,Long]().empty
   //var nc = 0
   val eulerphi2 = loadCoResPQ2//loadPQ2
   var eulerphi3 = Map[Long,Long]().empty
   var eulerphi4 = Map[Long,Long]().empty
   var eulerphi5 = Map[Long,Long]().empty
   var eulerphi6 = Map[Long,Long]().empty

   var nc = eulerphi2.size
   /*
   indices foreach {
     i => 
       val p = primes(i)
       var j = i + 1
       var q = primes(j)
       var pq = 1L*p*q
       while (pq < bound && j < (pl - 1)) {
	   val phi = (pq - p - q + 1L)
	   if ((pq - 1) % (p + q - 1) == 0) {
	     eulerphi2 = eulerphi2 + (pq -> phi)
	     //println("p = " + p + " q = " + q + " pq = " + pq + " phi = " + phi)
	     println(pq + " " + phi)
	     nc += 1
	   }
	   j += 1	   
	   q = primes(j)
	   pq = 1L*p*q
       }       
   }
   */

   println("2: few elems = " + eulerphi2.take(10) + " nc = " + nc)
   // fill in eulerphi(p1*p2*p3) i.e., eulerphi(p1)*eulerphi(p2*p3)       
   val candq2 = eulerphi2.keys.toList.sorted
   println("2: few elems-2 = " + candq2.take(10) + " nc = " + nc)
   val cl2 = candq2.length
   println("cl2 = " + cl2 + " maxvalue = " + candq2.max)
   indices foreach {
     i => 
     val p = primes(i)   
     var q = candq2(0)
     var pq = 1L*p*q     
     var j = 0
     while(pq < bound && j < (cl2 - 1)) {
	   val phi = 1L*(p - 1)*eulerphi2(q)
	   if (testCoresilience(phi, pq)&& (!eulerphi3.keys.toSet(pq))) {
	     eulerphi3 = eulerphi3 + (pq -> phi)
	     println("12: p = " + p + " q = " + q + " pq = " + pq + " phi = " + phi)
	     nc += 1
	   }
	   j += 1
	   q = candq2(j)
	   pq = 1L*p*q
       }        
   }
  
   if(eulerphi3.size < 1) 
     println("eulerphi3 is empty.")
   else {
     println("3: few elems = " + eulerphi3.take(10) + " nc = " + nc) 
     // fill in eulerphi(p1*p2*p3*p4) i.e., eulerphi2(p1*p2)* eulerphi2(p3*p4) and eulerphi(p1)*eulerphi3(p2*p3*p4)  
     List.range(0,cl2-1) foreach {
       i => 
	 val p = candq2(i)
       var j = i + 1
       var q = candq2(j)
       var pq = 1L*p*q
       while(pq < bound && j < (cl2 - 1)) {
	 val phi = eulerphi2(p)*eulerphi2(q)
	 if (testCoresilience(phi, pq) && (!eulerphi4.keys.toSet(pq))) {
	   eulerphi4 = eulerphi4 + (pq -> phi)
	   println("22: p = " + p + " q = " + q + " pq = " + pq + " phi = " + phi)
	   nc += 1
	 }
	 j += 1
	 q = candq2(j)
	 pq = 1L*p*q
       }        
     }

     val candq3 = eulerphi3.keys.toList.sorted
     val cl3 = candq3.length
     println("cl3 = " + cl3 + " maxvalue = " + candq3.max)
     indices foreach {
       i => 
       val p = primes(i)   
       var q = candq3(0)
       var pq = 1L*p*q
       var j = 0
       while(pq < bound && j < (cl3 - 1)) {
	 val phi = 1L*(p - 1)*eulerphi3(q)
	 if (testCoresilience(phi, pq)&& (!eulerphi4.keys.toSet(pq))) {
	   eulerphi4 = eulerphi4 + (pq -> phi)
	   println("13: p = " + p + " q = " + q + " pq = " + pq + " phi = " + phi)
	   nc += 1
	 }
	 j += 1
	 q = candq3(j)
	 pq = 1L*p*q
       }        
     }
   }

   if(eulerphi4.size < 1) println("eulerphi4 is empty.")    
   else {
     println("4: few elems = " + eulerphi4.take(10) + " nc = " + nc) 
     // fill in eulerphi(p1*p2*p3*p4*p5) i.e., (p1-1)*eulerphi4(p2*p3*p4*p5) or eulerphi2(p1*p2)*eulerphi3(p3*p4*p5)
     val candq4 = eulerphi4.keys.toList.sorted
     val cl4 = candq4.length
     println("cl4 = " + cl4)
     if (cl4 > 0) {
       indices foreach {
	 i => 
	   val p = primes(i)   
	 var q = candq4(0)
	 var pq = 1L*p*q
	 var j = 0
	 while(pq < bound && j < (cl4 - 1)) {
	   val phi = 1L*(p - 1)*eulerphi4(q)
	   if (testCoresilience(phi, pq) && (!eulerphi5.keys.toSet(pq))) {
	     eulerphi5 = eulerphi5 + (pq -> phi)
	     println("14: p = " + p + " q = " + q + " pq = " + pq + " phi = " + phi)
	     nc += 1
	   }
	   j += 1
	   q = candq4(j)
	   pq = 1L*p*q
	 }        
       }          
     }  
   }

   if(eulerphi3.size > 0) {
     val candq3 = eulerphi3.keys.toList.sorted
     val cl3 = candq3.length
     List.range(0,cl2) foreach {
       i => 
	 val p = candq2(i)
       List.range(0, cl3) foreach {
	 j => 
	   var q = candq3(j)
	 var pq = 1L*p*q
	 val phi = eulerphi2(p)*eulerphi3(q)
         if (pq < bound && testCoresilience(phi, pq)  && (!eulerphi5.keys.toSet(pq))) {
	   eulerphi5 = eulerphi5 + (pq -> phi)
	   println("23: p = " + p + " q = " + q + " pq = " + pq + " phi = " + phi)
	   nc += 1
	 }
       }
     }
   }
   
   if (eulerphi5.size < 1) 
     println("eulerphi5 is empty.")
   else {
     println("5: few elems = " + eulerphi5.take(10) + " nc = " + nc)
     // fill in eulerphi(p1*p2*p3*p4) i.e., eulerphi(p1)*eulerphi(p2*p3*p4)       
     val candq5 = eulerphi5.keys.toList.sorted
     val cl5 = candq5.length
     println("cl5 = " + cl5)
     indices foreach {
       i => 
	 val p = primes(i)   
       var q = candq5(0)
       var pq = 1L*p*q
       var j = 0
       while(pq < bound && j < (cl5 - 1)) {
	 val phi = 1L*(p - 1)*eulerphi5(q)
	 if (testCoresilience(phi, pq) && (!eulerphi6.keys.toSet(pq))) {
	   eulerphi6 = eulerphi6 + (pq -> phi)
	   //println("p = " + p + " q = " + q + " pq = " + pq + " phi = " + phi)
	   nc += 1
	 }
	 j += 1
	 q = candq5(j)
	 pq = 1L*p*q
       }        
     }
   }
  
   if(eulerphi6.size < 1) println("eulerphi6 is empty.")
   else println("6: few elems = " + eulerphi6.take(10) + " nc = " + nc)

   val fm = tests
   val fm2 = fm.filter(_._2.length == 2)
   val fm3 = fm.filter(_._2.length == 3)
   val fm4 = fm.filter(_._2.length == 4)
   val fm5 = fm.filter(_._2.length == 5)
   //println(fm2.foldLeft("")(_+"\n"+_) + "\n\n" + fm3.foldLeft("")(_+"\n"+_) + "\n\n" + fm4.foldLeft("")(_+"\n"+_) + "\n\n" + fm5.foldLeft("")(_+"\n"+_) )

   println("cores for 2 comp = " + fm2.size)

   val diff2 = (fm2.keys.toSet diff eulerphi2.keys.toSet)
   println("diff2: " + diff2.map{e => e.toString + " -> " + fm2(e).toString + "\n"} )
   if(diff2.size > 0) println("max prime: " + diff2.toList.map{e => fm2(e)}.flatten.max )

   println("cores for 3 comp = " + fm3.size)
   val diff3 = (fm3.keys.toSet diff eulerphi3.keys.toSet)
   if(diff3.size > 0) println("diff3: " + diff3.map{e => e.toString + " -> " + fm3(e).toString + "\n"} )

   println("cores for 4 comp = " + fm4.size)
   val diff4 = (fm4.keys.toSet diff eulerphi4.keys.toSet)
   if(diff4.size > 0) println("diff4: " + diff4.map{e => e.toString + " -> " + fm4(e).toString + "\n"} )

   println("cores for 5 comp = " + fm3.size)
   val diff5 = (fm5.keys.toSet diff eulerphi5.keys.toSet)
   if(diff5.size > 0) println("diff5: " + diff5.map{e => e.toString + " -> " + fm5(e).toString + "\n"} )

   println( "Time (s): " + ( System.currentTimeMillis() - start ) / 1. )
 }
}
