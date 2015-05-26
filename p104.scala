object P104 {

  def fast(n: BigInt): BigInt = {
    var a = BigInt(1)
    var b = BigInt(0)
    var c = BigInt(0)
    var d = BigInt(1)
    var i = n+1
    while(i > 0) {
      if(i % 2 == 1) {
        val t = (b + a) * d + (c * b)
        a = (c * a) + (d * b)
        b = t
      }  
      
      val t = (c * 2 + d) * d
      c = c * c + d * d
      d = t
      i /= 2
    }
    a
  }

def main(args: Array[String]) {
  val lim = 100000000
  val digits = List.range('1','9').toSet + '9'
/*
  var fib1 = BigInt(1)
  var fib2 = BigInt(1)
  var fibn = BigInt(0)
  var i = 3
  var found = false
  while(!found) {
      fibn = fib1 + fib2
      if (i < 10) println(i + " " + fibn)
      fib1 = fib2
      fib2 = fibn
      val fibnS = fibn.toString
      found = (fibnS.take(9).toSet == digits && fibnS.takeRight(9) == digits)
      if (!found) i+=1
  }
  if (i < lim) fibn
  */
  var found = false
  var i = 1000000
  while(!found) {
    fibnS = fast(i).toString
    found = (fibnS.take(9).toSet == digits && fibnS.takeRight(9) == digits)
    if (!found) i+=1
  }
}

}
