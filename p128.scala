object P128 {

def s(k: Long) = 3*k*k-3*k+2

def sdiamopposite(k: Long) = 3*k*k + 2

def neighS(k: Long) = Set(s(k-1), s(k+1), s(k)+1, s(k+1)+1, 2*s(k)+3, s(k+2)-1)

def diffS(k: Long) = Set(s(k-1)-s(k), s(k+1)-s(k), 1, s(k+1)+1-s(k), s(k)+3, s(k+2)-1-s(k))

def isPrime(l: Long) = primes.indexOf(l) != -1

def nprimesDiffS(k: Long) = Set(s(k-1)-s(k), s(k+1)-s(k), 1, s(k+1)+1-s(k), s(k)+3, s(k+2)-1-s(k)).filter(x => isPrime(math.abs(x))).size

  def enum(l: Int) = (1 to l) foreach { 
    k =>
      val v = nprimesDiffS(k)
    if (v == 3) println(k + " " + s(k)) 
  }

  def main(args: Array[String]) {
    val l = args(0).toInt
  }

}
