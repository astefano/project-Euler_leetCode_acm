def isPrime(n: Int) = n > 1 && !List.range(2, math.floor(math.sqrt(n)).toInt+1).exists(i => (n%i)==0)

def genPerm(n: Int) = {
	var i = 0
	var ns = n.toString
	var l = ns.length
	var sols = Set(n)	
	while (i < l) {
		ns = ns.tail + ns.head
		sols = sols + ns.toInt
		i = i + 1
	}
	sols
}

def isCircPrime(n: Int) = !genPerm(n).map{nx => isPrime(nx)}.exists(_==false)

//for numbers of 6 digits starting with 7 or 9
val digs = List(1,3,7,9)
for(d1 <- List(7,9); d2 <- digs; d3 <- digs; d4 <- digs; d5 <- digs; d6 <- digs; val x = d1*100000+d2*10000+d3*1000+d4*100+d5*10+d6; if (isCircPrime(x))) yield x
