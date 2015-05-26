object P111 {

  import scala.collection.mutable._

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

  def isPrime(n: Long) = n > 1 && !List.range(2, math.floor(math.sqrt(n)).toInt+1).exists(i => (n%i)==0)

  def isPrime0(n: Long) = if(witness(2,n) || witness(3,n)) false else true  
   
  def isPrime1(n: Long) = if(witness(2,n) || witness(7,n) || witness(61,n)) false else true  

  def isPrime2(n: Long) = if(witness(2,n) || witness(3,n) || witness(5,n) || witness(7,n) || witness(11,n)) false else true

  val lim = 4759123141L

  def main(args: Array[String]) {
    val ndigits = args(0).toInt

    val start = System.currentTimeMillis

    val digits = (0 to 9).toSet
    val groups = digits.subsets.toList.groupBy(_.size)

    val digitStack = Stack((0 to 9):_*)
    //val digitStack = Stack((1 to 2):_*)

    var s = 0L
    var allprimes = Set[Long]()

    while (!digitStack.isEmpty) {
      val d = digitStack.pop
      var found = false
      ((ndigits - 1) to 1 by -1).dropWhile {
	n => 
	val repeated = List.fill(n)(d)
	var cands = groups(ndigits-n).filter(s => !s(d)).map{s => s.toList ::: repeated}
	if (n == 8) 
	  cands = cands ::: digits.toList.map{d => List(d, d)}.map{l => l ::: repeated}
	var st = 0L
	var nt = 0
	//println("cands = " + cands)
	cands foreach {
	  c => 
	    val perms = c.permutations.toList
	    //println("perms = " + perms.toList)
	    val primes = perms.filter{
	      p => 		
		val pv = p.mkString.toLong
		//if (p.head!=0 && pv < lim) isPrime1(pv) else if (p.head!=0) isPrime2(pv) else false
		p.head != 0 && isPrime(pv)
	    }
	    //println("primes = " + primes.toList)
	    if (primes != List()) {
	      //if (d == 2) println("primes = " + primes.toList)
	      allprimes = allprimes ++ primes.map{_.mkString.toLong}.toSet
	      found = true	    
	      st += primes.map{_.mkString.toLong}.sum
	      nt += primes.length
	    }
	}
	println("d = " + d + ": n= " + n + " nt = " + nt + " st = " + st)
	s += st
	!found
      }
    }

    //println("some = " + allprimes.toList.take(10) + " size = " + allprimes.size)
    println("s = " + s)// allprimes.sum) 

    println( "Time (s): " + ( System.currentTimeMillis() - start ) / 1000. )

  }

}
