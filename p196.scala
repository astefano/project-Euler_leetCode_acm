object P196 {  
  //def isPrime(n: Long) = n > 1 && !List.range(2, math.floor(math.sqrt(n)).toInt+1).exists(i => (n%i)==0)

//from project euler forum problem 58 Miller-Rabin primality testing
def apower(a: Long, n: Long, mod: Long) = {
  var power = 1L*a //BigInt(a)
  var res = 1L //BigInt(1)  
  var na = n
  while(na!=0){
      if(na%2 == 0) res = (res*power) % mod
      power = (power*power) % mod
      na/=2
      }
  res
}
   
def witness(a: Long, n: Long) = {
   var t = 1L
   var u = n/2
   var i = 1
   var lln = n
   var prev = 0L
   var curr = 0L

   while(u%2==0){
      u/=2
      t+=1
   }
   prev = apower(a,u,n)
   while (i <= t) {
      curr = (prev*prev) % lln 
      if( (curr==1L) && (prev!=1L) && (prev!=lln-1L) ) true
      prev = curr
      i += 1
   }
   if(curr!=1L) true else false
}
   
  def isPrime(n: Long) = if(witness(2,n) || witness(7,n) || witness(61,n)) 0 else 1  

  //r is 4k + 1
  def c4k1(r: Int) = {
    var x = 1L*r*((r-1)/2) 
    //the value above 
    var xa = x - r
    //the value below right 
    var xbr = x + r 
    var c = 0
    //first el of row r
    var oldp = false
    if (isPrime(x+1)) {
      if (isPrime(xbr+2)) oldp = true
      if (oldp && isPrime(xa+2)) {
	println(x+1)
	c += 1
      }
    }
    var j = 3
    while (j < r-1) {
      x += 2  
      xbr += 2
      xa += 2
      if (isPrime(x)) {
	if (oldp) {
	  if (isPrime(xbr)) {
	    println("found x = " + x)
	    c += 1
	  }
	  else {
	    oldp = false
	    if (isPrime(xa)) {
	      println("found x = " + x)
	      c += 1
	    }
	  }
	}
	    else {
	      if (isPrime(xbr)) {
		if (isPrime(xa)) {
		  println("found x = " + x)
		  c += 1
		}
	      }
		else oldp = false
	    }
      }     
      j += 2
    }
    println(c)
  }	

  def main(args: Array[String]) {
    val start = System.currentTimeMillis
    c4k1(9)
    c4k1(21)
    c4k1(5678027)
    println( "Time (s): " + ( System.currentTimeMillis() - start ) / 1000. )
  }
}
