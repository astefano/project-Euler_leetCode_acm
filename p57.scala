object P57{

  def main(args: Array[String]) {

    var i = 0 
    var c = 0
    var f = (BigInt(5),BigInt(2))
    while(i < 1000) {
      val nf = (2*f._1 + f._2, f._1)
      val vnf = (nf._1 - nf._2, nf._2)
      if (i < 10) println(vnf)
      if (vnf._1.toString.length > vnf._2.toString.length) c += 1
      i += 1
      f = nf
    }
    println(c)
  }
}
