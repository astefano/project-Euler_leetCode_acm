object P207 {

// http://www.codecodex.com/wiki/Calculate_an_integer_square_root
  def sqrt(number : BigInt) = {
    def next(n : BigInt, i : BigInt) : BigInt = (n + i/n) >> 1

    val one = BigInt(1)

    var n = one
    var n1 = next(n, number)
	  
    while ((n1 - n).abs > one) {
      n = n1
      n1 = next(n, number)
    }
	   
    while (n1 * n1 > number) {
      n1 -= one
    }
	   
    n1
  }

val epsilon = 0.000000000000001

def log2(x0: BigInt) = {
    var x = x0

    var y = 0.0
    var z = 1.0
    var f = 0.0
    while( x >= 2 )
    {
	x = x / 2
        if( x % 2 == 1) f += 1.0
        f /= 2.0
        y+=1
    }
    f += 1.0
    while( z > epsilon )
    {
        f *= f
        z /= 2.0
        if( f >= 2.0 )
        {
            y += z
            f /= 2.0
        }
    }
    y
}

def isNat(d: Double) = d.toLong == d

val lim: Double = 1.* scala.Long.MaxValue * scala.Long.MaxValue 

val wanted = 1./12345

	def main(args: Array[String]) {
	println("lim = " + lim)

	var ni = 1.
	var all = 1
	var k = 5.
	
	while(ni/all >= wanted && k < lim) {	      
	      val t2 = math.sqrt(k+0.25) + 0.5
	      if (isNat(t2)) { 
		all += 1
		val t = math.log(t2)/math.log(2)
		if (isNat(t)) ni += 1 
		}
	      if (k == 20 || k == 180 || k == 185) println("P(" + k + ")=" + ni + "/" +all)
	      k += 1	
	      }
        println("P(" + k + ")=" + ni + "/" +all)

	}
}

