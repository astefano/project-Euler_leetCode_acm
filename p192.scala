
object P192 {

  def bestApprox(x: BigInt, bound: Long) = {
    var rb = (1L,1L)
    var l = (0L,1L)
    var r = (1L,0L)
    var next = (l._1 + r._1, l._2 + r._2)  
    var besterr = (x,BigInt(1))
    while (next._2 < bound) {
      val vn = 1.*next._1/next._2
      val bign1 = BigInt(next._1)
      val bign2 = BigInt(next._2)
      val bign1S = bign1*bign1
      val bign2S = bign2*bign2
      val bign2Sx = x*bign2S
      val abs1 = if (bign1S > bign2Sx) (bign1S - bign2Sx) else (bign2Sx - bign1S)
      /* we save the diff next._1/next._2 - sqrt(x) as
       * the pair nerr = (abs(next._1^2 - next._2^2*x), next._2^2)
       * so we have a better approx when:
       * besterr._1*nerr._2 > nerr._1*besterr._2
       */ 
      val nerr = (abs1,bign2S)
	
      if (besterr._1*nerr._2 >= nerr._1*besterr._2) {
	rb = next
	besterr = nerr
      }
      /* next._1/next._2 must approx sqrt(x), i.e.,
       * next._1^2 approx next._2^2 * x
       * so we test for
       * 
       */ 
      if (bign1S > bign2Sx) r = next
      else l = next
      //println("next = " + next + " diff = " + nerr)
      next = (l._1 + r._1, l._2 + r._2)  
    } 
    println("x = " + x + " rb = " + rb)
    rb
  }

  def main(args: Array[String]) {
    val start = System.currentTimeMillis
    //bestApprox(5, math.pow(10,12).toLong)
    val notsquare = List.range(2,100001).filter(x => math.floor(x/math.sqrt(x)) != math.sqrt(x))
    val res = notsquare.foldLeft(BigInt(0))(_+
    bestApprox(_, math.pow(10,12).toLong)._2)
    println("res = " + res)
    
    println( "Time (s): " + ( System.currentTimeMillis() - start ) / 1000. )
  }
}

