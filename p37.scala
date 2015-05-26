def genTrunc(n: Int) = {
	var i = 0
	var nsl = n.toString
	var nsr = n.toString
	var l = nsl.length
	var sols = Set(n)	
	while (i < l - 1) {
		nsl = nsl.drop(1)
		nsr = nsr.dropRight(1)
		sols = sols + nsl.toInt + nsr.toInt
		i = i + 1
	}
	sols
}

def isTruncPrime(n: Int) = !genTrunc(n).map{nx => isPrime(nx)}.exists(_==false)
val digs = List(1,3,7,9)
for(d2 <- digs; d3 <- digs; d4 <- digs; d5 <- digs; d6 <- digs; val x = d2*10000+d3*1000+d4*100+d5*10+d6; if (isTruncPrime(x))) yield x
