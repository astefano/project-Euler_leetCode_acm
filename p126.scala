import scala.util.control.Breaks._

object P126 {

  def nc(a: Int, b: Int, c: Int, n: Int) = 2*(a*b+a*c+c*b) + 4*(n-1)*(a+b+c+n-2)

  def main(args: Array[String]) {

    val l = args(0).toInt

    val nmax = 6*l*l + 4*(l-1)*(4*l-2)+1

    val lt = 30

    val mv = 20000

    val m = Array.fill(mv)(0)

    for(n <- 1 to 100) {
      for(a <- 1 to l; if (nc(a,a,a,n) < mv)) {
	for(b <- a to l; if (nc(a,b,b,n) < mv)) {
	  for(c <- b to l) {
	    val v = nc(a,b,c,n)
    	    if (v < mv) m(v) += 1
	      //println(a + " " + b + " " + c)
	  }
	}
      }
    }
    
    println(m.max + " " + m.indexOf(1000))
  }

}
