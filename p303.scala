object P303 {

  //val f = Map(1->1L, 2->2L, 3->12L, 4->12L, 5->10L, 6->12L, 7->21L, 10->10L)

  val lim = 10000

  val stop = 100000000
  
  val f = Array.fill(lim+1)(BigInt(0))

  f(1) = 1L; f(2) = 2L; f(3) = 12L; f(4) = 12L
  f(9) = BigInt(12222)
  f(99) = BigInt(1122222222)
  f(999) = BigInt("111222222222222")
  f(9999) = BigInt("11112222222222222222")
  //f(1998) = 1L
  //f(2997) = 1L

  def isOK(l: Long) = l.toString.toSet subsetOf Set('0','1','2')

  def isOK(l: BigInt) = l.toString.toSet subsetOf Set('0','1','2')

  def upd0(n: Int) = {
    if (f(n) == 0) {
      var min = BigInt(n)
      val r = (1 to n-1).foreach{
	i => 
	  if (f(i) % n == 0 && min > f(i)) min = f(i)
	  val cand5 = f(i)*5
  	  if (isOK(cand5) && cand5 % n == 0 && min > cand5) min = cand5
	  val cand10 = f(i)*10
  	  if (cand10 % n == 0 && min > cand10) min = cand10
      }
      if (min == n) {
	var candDiv = BigInt(n)
	var counter = 1
	while(!isOK(candDiv) && counter < stop) {
	  candDiv += n 
	  counter += 1
	}
	if (counter < stop) f(n) = candDiv else {
	  println("stopped for " + n)
	  f(n) = 1L
	}
      }
      else {//maybe there's a multiple smaller than min
	var candDiv = BigInt(n)
	while(!isOK(candDiv) && candDiv <= min) {
	  candDiv += n
	}
	f(n) = if (candDiv < min) candDiv else min
      }
    }
    println("f("+n+")="+f(n))
    f(n)
  }

  def upd(n: Int) = {
    var candDiv = n
    var counter = 1
    while(!isOK(candDiv)) {
      counter += 1
      candDiv = n*counter
    }
    f(n) = candDiv
    f(n)
  }

  def updS(n: Int) = {
    var candDiv = BigInt(n)
    var counter = 1
    while(!isOK(candDiv)) {
      counter += 1
      candDiv += n
    }
    println(counter)
    candDiv
  }

  def main(args: Array[String]) {

    val start = System.currentTimeMillis       

    val res = (1 to lim).foldLeft(BigInt(0))((r,c) => r + upd0(c)/c)
    
    println("res=" + res)

    //println("f(42) = " + f(42) + "; f(89) = " + f(89))
    //println(f.toList)

    println( "Time (s): " + ( System.currentTimeMillis() - start ) / 1000. )
  }
} 


//old res: res=1111444886800363

/*
stopped for 1998
stopped for 2997
stopped for 3996
stopped for 4995
stopped for 5994
stopped for 6993
stopped for 7992
stopped for 8991
stopped for 9899
stopped for 9989
stopped for 9990
res=1111445661649903
Time (s): 3854.885
*/
