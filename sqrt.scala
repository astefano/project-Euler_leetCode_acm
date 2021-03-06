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
