object P193 {

  import scalax.io._
  import scalax.file.{ FileOps, Path, NotFileException }

  def naivetest(lim : Int) = {
    val bound = math.pow(2,lim).toInt
    val p = List(2,3,5,7,11,13,17,19,23,29,31,37,41,43)  
    val p2 = p.filter(x => x*x < bound) map {pi => pi * pi}
    val bad = List.range(4,bound+1).filter(cand => p2.foldLeft(false)((r,c)=>(r||(cand % c == 0))))
    println("bad = " + bad)
    bad.length
  }

  def main(args: Array[String]) {
    var limit = math.pow(2,args(0).toInt).toLong
    var sqrtLim = math.pow(2,args(0).toInt/2).toLong
    val start = System.currentTimeMillis
    val file = "/media/ubuntuPart2/docs/topcoder/allprimes.txt"
    val rf: Input = Resource.fromFile(file)

    val lines = rf.lines().filter(_!="")//.take(2)
    val nlines = lines.size
    println("n lines = " + nlines)
    var i = 0
    //count 4
    var count = 1L
    var visitedPrimes = List(2)

    lines foreach {
    pli => 
      val cprimes = pli.trim.split(" ").filter(_!="").map(_.toInt)
      cprimes foreach {
	pc => 
	  val prev = visitedPrimes.last
	  if (prev <= sqrtLim) {
	    //if (count > 900) println(" pc = " + pc + " prev = " + prev + " visited.length = " + visitedPrimes.length + " sl = " + sqrtLim) 
	    visitedPrimes foreach {
	      x =>
		val ml = (1.*prev/x)
		val mr = (1.*pc/x)
		val ml2 = ml*ml
	        val temp = (1.*sqrtLim/x)
		val lim2 = temp*temp
	        val mr2 = math.min(mr*mr, lim2)
		val t = (math.floor(mr2) - math.floor(ml2))	         
		count += t.toLong
	      //println("   x = " + x + "; (ml, mr) = " + (ml2, mr2) + ". t = " + t + " count = " + count + "; badaux = " + List.range(math.floor(ml2).toInt + 1, math.floor(mr2).toInt + 1).map(k => k *x *x)) 
	    }	    
	    visitedPrimes = visitedPrimes :+ pc
	    count += 1
	  }
      }
  }
    count -= 1
    val res = limit - count
    var nc = 0
    //nc = naivetest(args(0).toInt)
    println("count = " + count + "; naive count =  " + nc + " res = " + res)

    println( "Time (s): " + ( System.currentTimeMillis() - start ) / 1000. )
  }
}

/*
 p foreach {
   pi => 
     val pl = vis.last
     println("pi = " + pi + " prev = " + pl)
     vis foreach {
       x =>
	 val ml = (1.*pl/x)
	 val mr = (1.*pi/x)
	 //val t1 = ((mr - ml)*(mr + ml))//.toInt
	 val t2 = (math.floor(mr*mr) - math.floor(ml*ml))
	 c += t2.toLong
	 //println("   x = " + x + "; (ml, mr) = " + (ml*ml, mr*mr) + ". t1 = " + t1 + " t2 = " + t2 + " c = " + c) 
     }
     vis = vis :+ pi
     //add not-square pi^2
     c += 1
 }
*/
