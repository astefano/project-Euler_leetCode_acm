object P148 {

  val lim = 1000000000

  def f(l: Int) = {
    var nf = 28
    var m = new Array[Int](3)
    m(0) = 6
    m(1) = 15
    m(2) = 20
    List.range(7, l) foreach {
      li => 
	nf += 2
      val ml = m.length
      val nml = if (li % 2 == 1) ml + 1 else ml
      val nm = Array.fill(nml)(0)
      //first new elem = 1 + m(0)
      nm(0) = 1 + m(0)
      if (nm(0) % 7 != 0) nf += 2
      List.range(0, ml-2) foreach {
	i => 	
	val x = m(i) + m(i+1)
	nm(i+1) = x 
	if (x % 7 != 0) nf += 2
      }
      //first new elem = 1 + m(0)
      val last = m.takeRight(2).sum
      if (last % 7 != 0) {
	if (li % 2 == 0) nf += 2 else nf += 1
      }
      nm(ml-1) = last
      nm(nml - 1) = last
      m = nm 
      println(li + ": " + m.toList + " nf = " + nf)
    }
    nf
  }

  //gen the line n in pascal triangle: el(0) = 1, el(k+1) = el(k) * (n-k)/(k+1)
  def g(l: Int) = {
    var nf = BigInt(6)
    List.range(3, l) foreach {
      li => 
	print(li + ":")
	val liEl0 = BigInt(1)
	var liElC = liEl0
	nf += 2
	val middle = li/2
	List.range(0, middle) foreach {
	  k => 
	    val liElk = liElC * (li - k) / (k + 1)
	    liElC = liElk
	    print(liElC + " ")
	    if (liElC % 7 != 0) nf += 2
	}
      val liElLast = liElC * (li - middle) / (middle + 1)
      if (liElLast % 7 != 0 && li % 2 != 0) nf -= 1
      print(liElLast + "\n")// + " " + (if (li % 2 == 0) liElLast else "") + "\n")
    }    
    nf
  }

  def main(args: Array[String]) {
    val n = args(0).toInt
    val start = System.currentTimeMillis
    val r = g(n)
    println(r)
    println( "Time (s): " + ( System.currentTimeMillis() - start ) / 1000. )
    //f(100)
    //f(1000000000)
  }

}
