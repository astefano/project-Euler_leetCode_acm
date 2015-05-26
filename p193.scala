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

    /* mobius fct calc taken from:
     * http://mathoverflow.net/questions/99473/calculating-mobius-function
     */
    val sqrt2 = math.floor(math.sqrt(sqrtLim))
    i = 2
    while (i <= sqrt2) {
        if (mu(i) == 1)
        {
	    var j = i
	    while (j <= sqrtLim) {
              mu(j) *= -i
	      j += i
	    }
	    j = i * i
	    while (j <= sqrtLim) {
              mu(j) = 0	      
	      j += i * i
	    }
        }
      i += 1
    }
    i = 2
    while (i <= sqrtLim) {
        if (mu(i) == i)
            mu(i) = 1
        else if (mu(i) == -i)
            mu(i) = -1
        else if (mu(i) < 0)
            mu(i) = 1
        else if (mu(i) > 0)
            mu(i) = -1
      i += 1
    }

    println("mu.take(10)" + mu.take(10).toList + " last mu = " + mu(sqrtLim.toInt))
   
    var count = 0L
    i = 1
    while(i <= sqrtLim) {
      count += math.ceil(limit/i/i).toLong*mu(i)
      i += 1
    }

    println("count = " + count)
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
