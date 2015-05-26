var max = 955

val l = 10

def p56(l: Int) = {
var a = 2
var nx = x
var max = 0
while(a < l) {
      val x = BigInt(a)
      nx = x
      List.range(2,l).foreach{
	b => 
	nx = x*nx
        val nmax = nx.toString.foldLeft(0)(_+_-'0')	
	if (nmax > max) {
	println("new one found: " + (a,b) + " sum = " + nmax + "(a^b = " + nx + ")")
	max = nmax
	}
	}
      a += 1
}
      println("last: " + (a-1,l-1) + "(a^b = " + nx + ", with sum = " + nx.toString.foldLeft(0)(_+_-'0') + ")")
}

new one found: (99,95) sum = 972(a^b = 3848960788934848611927795802824596789608451156087366034658627953530148126008534258032267383768627487094610968554286692697374726725853195657679460590239636893953692985541958490801973870359499)


