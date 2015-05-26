object P53{

  def genCombMatrix(n: Int) = {
  //comb(n)(k) = C_n^k
    val comb = Array.ofDim[Long](n,n)//[BigInt](n,n)
    val indices = List.range(0,n)
    indices foreach {
      i => 
      comb(i)(0) = 1L//BigInt(1)
      comb(i)(i) = 1L//BigInt(1)
      comb(i)(1) = 1L*i//BigInt(i)
    }
    List.range(2,n) foreach {
      i => 
      List.range(2,i) foreach {
	k => 
	  comb(i)(k) = comb(i-1)(k-1) + comb(i-1)(k)
      }
    }
    comb
  }

  def main(args: Array[String]) {
    val n = args(0).toInt
    val combs = genCombMatrix(n+1)
    println(combs(n-1)(n/2) + "\n" + combs.toList.map{_.toList}.flatten.filter(x => x>1000000 || x<0).length)
  }

}
