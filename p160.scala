import scala.sys.process._
import scalax.io._
import scalax.file.{ FileOps, Path, NotFileException }

object P160 {

//val lim = math.pow(10,2).toLong
val lim = 10L

val mod = math.pow(10, 5).toInt
//val mod = math.pow(10, 2).toInt

def naive(l: Long) = trimZeros(List.range(1L, l+1).reduceLeft((r,c) => (trimZeros(r) % mod) * c)) % mod

def naive2(l: Long) = {
  var r = 1L
  var i = 1
  while(i <= l) {
    val or = r
    r = (trimZeros(r) % mod) * i
    if (i >= 280) println("2: r = " + r + " or = " + or + " i = " + i)
    i += 1
  }
  trimZeros(r) % mod
}

//val m = Array.tabulate(mod, mod)((x,y) => 0)

//while(i < 101) {val i5 = (i % 1000000).toInt; f*= i5; while (f % 10 == 0) f = f/10; f = f % 1000000; i += 1 }

/*
  def f(l: Long, r: Long) = {
    if (r == l + 1) {
      if (m(l)(r) != 0) m(l)(r) 
      else {
	val res = l*r % mod
	m(l)(r) = res
      }
    }
      else {
	val middle = (l + r) / 2 
	val res = f(l, middle)*f(middle+1, r) % mod
	if (m(l)(r) == 0) m(l)(r) = res
	res
      }
  }
*/

  def trimZeros(l0: Long) = {
    var l = l0
    while (l > 1 && l % 10 == 0) l = l/10
    //println("l0 = " + l0 + " l = " + l)
    l
  }

/*
  def trimZeros(l0: Int) = {
    var l = l0
    while (l > 1 && l % 10 == 0) l = l/10
    l
  }
*/

  def g(l: Long, r: Long): Long = {
    if (l == r) trimZeros(l)
    else if (r == l + 1) {
     val res =  trimZeros((trimZeros(l) % mod) * (trimZeros(r) % mod)) % mod
     //println("[" + l + ", " + r + "] = " + res)
     res
    }
    else {
	val middle = (l + r) / 2 
        //println("[" + l + ", " + r + "]")
	trimZeros((trimZeros(g(l, middle)) % mod) * (trimZeros(g(middle+1, r)) % mod)) % mod
      }
  }

/*
  def modPow(a: Int, x: Int): Int = {
    if (x == 0) 1
    else if (x == 1) a
    else { 
      val na = trimZeros(a) % mod
      val lx = x/2
      println("na = " + na + " x = " + x)
      trimZeros((trimZeros(modPow(na, lx)) % mod) * (trimZeros(modPow(na, x-lx)) % mod)) % mod
     }
  }
*/

  def modPowT(a: Long, x: Long): Long = {
    if (x == 0) 1
    else if (x == 1) a
    else { 
      val na = trimZeros(a) % mod
      val lx = x/2
      val l = (trimZeros(modPowT(na, lx)) % mod)
      val r = (trimZeros(modPowT(na, x-lx)) % mod)	
      val res =       trimZeros(l * r) % mod
      //println("na = " + na + " x = " + x + " l = " + l + " r = " + r + " res = " + res + " diff = " + (res - (a*a % mod)))
      res
     }
  }

 def fact() = {
   val cmd = ("/media/ubuntuPart2/docs/topcoder/primegen-0.97/./primes 7 " + lim)
/*	
   val out = new StringBuilder
   val err = new StringBuilder

   val logger = ProcessLogger(
     (o: String) => out.append(o),
     (e: String) => err.append(e))
   
   cmd ! logger  #> file("/media/ubuntuPart2/docs/topcoder/primesP160.txt")

//   val all = out.toString
   println("cmd = " + cmd + " out = " + out)
   */

  //res = 2^(l/2-l/5)*3^(l/3) because each 5 pairs with a 2 to get a 10 which we can ignore
 
  var res = trimZeros((modPowT(2L, lim/2-lim/5)*modPowT(3L, lim/3)) % mod)
  //var res = math.pow(2, lim/2-lim/5)*math.pow(3, lim/3)
  println("res = " + res)

  val lines: Stream[String] = cmd lines_!

  lines foreach {
    l => 
      val p = l.trim.toLong
      var maxp = p
      while(maxp < lim) maxp = maxp * p// maxp
      res = trimZeros((res * p) % mod)
      //res = res*math.pow(p, exp)//trimZeros((res * modPowT(p, exp)) % mod)
  }
  res
 }

   def bfac(l: Int, r: Long) = List.range(l, r+1).foldLeft(BigInt(1))(_*_)

   def lastK(x: BigInt, k: Int) = x.toString.reverse.dropWhile(_=='0').take(k).reverse.toLong

   def bpow(b: Int, exp: Int) : BigInt = {
     if (exp <= 0) 1
     else if (exp == 1) b
     else bpow(b, exp/2) * bpow(b, exp-exp/2) 
   }

   def t(l: Long) = {
     var p = 0
     val lp = math.floor(math.log(l)/math.log(5)).toInt
     //val lastp5 = bpow(5, lp)
     val lk = math.floor(math.log(l)/math.log(2)).toInt
     println("(lp, lk) = " + (lp,lk))
     var res = BigInt(1)
     while(p <= lp) {
       var k = 0
       while(k <= lk) {
	 var x = 0
	 val pk = if (k > p) bpow(2, k-p) else bpow(5, p-k)
	 val lx = math.floor(l/math.pow(5,p)/math.pow(2,k))
	 println("(p, k, lx, pk) = " + (p, k, lx, pk))
	 while(5*x < lx) {
	   List.range(1,5) foreach {	  
	     i => 
	       val nx = (5*x+i)
		 if (nx % 2 == 1 && nx <= lx) {
		   val nnx = nx*pk
		   if (nnx <= l) {
		   //println("at (p, k, nnx, nx, x) = " + (p,k,nnx,nx,x))
		     if (nx > 1) {
		       println("added (p, k, nnx, x) = " + (p,k,nnx,x))
		       res *= nnx
		     }
		     else if ((k-p > lp) || (p == lp && (k - p) == lp) ) {
		       println("added (p, k, nnx, x) = " + (p,k,nnx,x))
		       res *= nnx
		     }
		   }
		 }
	   }
	   x+=1
	 }
	 k += 1
       }
       p += 1
     }
     res
   }

def main(args: Array[String]) {

//(List.range(1,21):::List(25, 42, 50, 70, 100,450, 1000)).map{i => println("f(" + i + ") = " + g(1, i))}
  val lim = args(0).toLong

  val r1 = naive(lim)
  println("r1 = " + r1)

  val r2 = bfac(1,lim)
  println("r2 = " + r2)

  val r = t(lim)
  println("r = " + r)
}

}
