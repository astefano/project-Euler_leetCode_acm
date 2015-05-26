object P78 {

 //ways to write n as a1 + a2 + a3 with start <= a1 < a2 < a3 
 def w3(n: Int, start: Int) = List.range(start, n/3+1).foldLeft(0)((s, a1) => s + (n-a1)/2 - a1 + 1)

 /* ways to write n as a1 + ... + ak with start <= a1 < ... < ak
  * in general: 1 <= a1 <= n/k, a1 <= a2 <= (n-a1)/(k-1) ... a(k-2) <= a(k-1) <= (n-a1-..-a(k-2))/2 and ak = n - (a1 + ...+ a(k-1))
  * so => 
  * wf(n, k, 1) = \sum_{a1=1}^[n/k] \sum_{a2=a1}^[n-a1/(k-1)] \sum_{a3=a2}^[n-a1-a2/(k-2)] ... \sum_{a(k-2)=a(k-3)}^[n-a1-...-a(k-3)/2] (n-a1-...-a(k-2))/2 - a(k-2) + 1
  * and the rec formulation is next. 
  */ 
 def wf(n: Int, k: Int, start: Int): Long = if (k == 3) w3(n, start) else List.range(start, n/k+1).foldLeft(0L)((s, a1) => s + wf(n-a1, k-1, a1))

  def pc(n: Int, x:Int) = {
    var i = 1
    var xi = BigInt(x)
    var res = xi/(xi-1)
    while(i < n) {
      xi = xi*x
      res = res*xi/(xi-1)
      i+=1
    }
    val xn = xi*x
    var aux = 
    res = xn*res*(xn/(xn-1) - 1)
    res
  }


 def f(k: Int) = if(k > 0) k*(3*k-1)/2 else (-1)*k*(-3*k+1)/2

 def main(args: Array[String]) {
  val lim = 100000
  val p = Array.ofDim[BigInt](lim+1)
  p(0) = BigInt(1)
  p(1) = BigInt(1)
  p(2) = BigInt(2)
  p(3) = BigInt(3)
  p(4) = BigInt(5)
  p(5) = BigInt(7) 
  var n = 5 
  while (n < lim  && (p(n) % 1000000 != 0)) {
    n += 1
    var k = 1
    var fk1 = f(k)
    var fk2 = f(-k)
    p(n) = 0
    while (fk1 <= n) {
      val sign = if ((k+1) % 2 == 0) 1 else -1
      if (fk2 <= n)
	p(n) += sign*(p(n-fk1) + p(n-fk2))
      else 
	p(n) += sign*p(n-fk1)
      //println("n: " + n + " k = " + k + " fk1 = " + fk1 + " fk2 = " + fk2 + " pn = " + p(n))
      k += 1
      fk1 = f(k)
      fk2 = f(-k)
    }
  }
  
  println(n + " " + p(n))// + " " + p.take(n+1).toList)

 }
}
