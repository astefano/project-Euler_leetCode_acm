object P198 {
  //from a continued fraction [0; a1...an] get the corresponding rat value
  def getRat(cf : List[Int], rf : (Int, Int)) : (Int, Int) = {
    if (cf.length == 1) (rf._2, cf(0)*rf._2 + rf._1)
    else getRat(cf.dropRight(1), (rf._2, cf.last*rf._2 + rf._1))
  }

  def getRat(cf : List[Int], rf : (Long, Long)) : (Long, Long) = {
    if (cf.length == 1) (rf._2, cf(0)*rf._2 + rf._1)
    else getRat(cf.dropRight(1), (rf._2, cf.last*rf._2 + rf._1))
  }

  def getRat(cf : List[Int], rf : (BigInt, BigInt)) : (BigInt, BigInt) = {
    if (cf.length == 1) (rf._2, cf(0)*rf._2 + rf._1)
    else getRat(cf.dropRight(1), (rf._2, cf.last*rf._2 + rf._1))
  }

  //contfr for sqrt(n)
  def getContFrSqrt(n: Int) = {
    var m = 0.
    var d = 1.
    var a0 = math.floor(math.sqrt(n))
    var a = a0
    var mdp = Set((m, d, a))
    var cf = List(a)
    var i = 0
    do {
      println((m,d,a))
      m = d*a - m
      d = (n - m*m)/d
      a = math.floor((1.*a0 + m)/d)
      cf = cf :+ a      
      i += 1
    }while(!mdp((m,d,a)) && i < 30)
    cf.map{_.toInt}
  }

  def bestApprox(a: BigInt, b: BigInt, bound: Int) = {
    var bestapp1 = (0,0)
    var bestapp2 = (0,0)
    var l = (0,1)
    var r = (1,0)
    var next = (l._1 + r._1, l._2 + r._2)  
    var besterr1 = (BigInt(1), BigInt(1)) //(a, b)
    while (next._2 <= bound) {
      val vn = 1.*next._1/next._2
      // we rewrite (vn - a/b) as a pair (next._1 * b - next._2 * a, next._1 * b)
      val next1b = b*next._1
      val next2b = b*next._2
      val next2a = a*next._2
      val absAux = (next1b - next2a).abs
      val newerr = (absAux, next2b)	
      // we rewrite (besterr >= newerr) as besterr._1 * newerr._2 >= newerr._1 * besterr._2 
      val aux1 = besterr1._1 * newerr._2
      val aux2 = newerr._1 * besterr1._2
      if (aux1 >= aux2) {	
	bestapp2 = bestapp1
	bestapp1 = next
	/* if | a/b - next | < | a/b - bestapp1 | then bestapp1 is invalidated (it can no longer be a best approx)
	 */ 
	val aux3 = (next2a - next1b).abs*bestapp2._2
	val aux4 = (a*bestapp2._2 - b*bestapp2._1).abs*next._2
	if (aux3 < aux4) bestapp2 = (0,0)
        println("bestapp1 = " + bestapp1 + " bestapp2 = " + bestapp2 + " aux1 = " + aux1 + " aux2 = " + aux2 + " besterr1 = " + besterr1 + " newerr = " + newerr)	
	besterr1 = newerr
      }
      // we rewrite (vn > a/b) as (next._1*b > next._2*a) 
      if (next1b > next2a) r = next
      else l = next
      //println("next = " + next + " diff = " + newerr + " aux1 = " + aux1 + " aux2 = " + aux2)
      next = (l._1 + r._1, l._2 + r._2)  
    } 
    (bestapp1, bestapp2)
  }

//  bestApprox(9./40,6)


def farey(n : Int, asc : Boolean ) = {
// Adapted from """Python function to print the nth Farey sequence, either ascending or descending."""
	var i = 0 
        var a = 0 
        var b = 1
        var c = 1
        var d = n         
    if (asc) {      
        a = 0 
        b = 1
        c = 1
        d = n     
    }
    else {
	a = 1
        b = 1
	c = n-1
	d = n     
    }
    println(a + "/" + b)
    while ((asc && c <= n) || (!asc && a > 0)) {
	//i += 1
	val k = scala.math.floor((1.*n + b)/d).toInt
	val ao = a
	val bo = b
	a = c
        b = d
	c = k*c - ao
	d = k*d - bo
        //if (b > 30) 
       println(a + "/" + b)
    }
}


farey(8,true)

//using Stern Brocot tree, find the best approx for x which is greater than x and within the bound 
//e.g., for 9/40 it is 1/4
def findRB(x : (Int, Int), bound : Int) = {
  val vx = 1.*x._1/x._2
  var rb = (1,1)
  var l = (0,1)
  var r = (1,1)
  var next = rb
  var i = 0
  while (next._2 < bound) {
    i += 1
    //println("next = " + next)
    next = (l._1 + r._1, l._2 + r._2)
    val vn = 1.*next._1/next._2
    if (1.*rb._1/rb._2 > vn && vn > vx) rb = next
    if (vn > vx) r = next
    else l = next
  }  
  //println("i = " + i)
  rb
}

//findRB((9,40), 6)

def countAmbiguous(n : Int, bound : Int) = {
// Adapted from """Python function to print the nth Farey sequence, either ascending or descending."""
	var camb = 0
	var i = 0 
        var a = 0
        var b = 1
        var c = 1
        var d = n         
	var lb = (0,1)
	var rb = (1,1)
    while (c <= n) {
	i += 1
	val ao = a
	val bo = b
	if (bo < bound && 1.*ao/bo > 1.*lb._1/lb._2) lb = (ao,bo)
	val k = scala.math.floor((1.*n + b)/d).toInt
	a = c
        b = d
	c = k*c - ao
	d = k*d - bo
	rb = findRB((a,b), bound)
	val f = 1.*a/b
        if (f < 1./100 && 2.*f == 1.*lb._1/lb._2 + 1.*rb._1/rb._2) 
	  println(a + "/" + b + "(" + lb + ", " + rb + ")")	
    }
  println("i = " + i)
}

//countAmbiguous(40,6)

def main(args: Array[String]) {
    val a = BigInt(args(0))
    val b = BigInt(args(1))
    val bound = args(2).toInt
    val start = System.currentTimeMillis
    val res = bestApprox(a, b, bound)
    //val res = bestApprox(a, b,math.pow(10,8).toInt)
    println("res = " + res)
    
    println( "Time (s): " + ( System.currentTimeMillis() - start ) / 1000. )
  }
}

