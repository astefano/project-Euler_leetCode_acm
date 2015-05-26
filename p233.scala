object P233 {

def gcd(a: Int, b: Int): Int = if(b == 0) a else gcd(b, a % b)

def countKs(v: Long) = {
  var k = if (v % 2 == 0) 0 else 1
  var d = 1
  var n = 1
  var m = 2
  var limd = v
  var ck = 0
  while (d < limd) {
    if (v % d == 0) {
      val vd = v/d
      while (n < math.sqrt(vd)) {
	m = n + 1
	while (m < 2*math.sqrt(vd)) {
	  if ((gcd(n,m) ==1) && ((n*n + m*m) == 2*vd)) ck += 1
	  m += 1
	}
	n += 1
      }
    }
    d += 1
  }
  ck
}

def main(args: Array[String]) {
  println(countKs(10000))
}

}
