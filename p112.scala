object P112 {

  def bouncy(ns : String) : Boolean = {    
    val l = ns.length
    //println(ns)
    if (l <= 1) false
    else {
      val indices = List.range(1, l-1)
      if (ns(0) < ns(1)) {
	val r = indices.dropWhile{
	  i => 
	    ns(i) <= ns(i+1)
	}
	//println(r)
	(r!=List())
      }
      else if (ns(0) > ns(1)) {
	val r = indices.dropWhile{
	  i => 
	    ns(i) >= ns(i+1)
	}
	//println(r)
	(r!=List())
      }
      else bouncy(ns.drop(1))
    }
  }

  def main(args: Array[String]) {

    var i = 101 
    var proc = 0.
    var nbouncy = 0
    var ord = List[Int]()
    var found = false
    while (!found && i < 200000) {
      if (bouncy(i.toString)) { 
	nbouncy += 1
	proc = 100. * nbouncy / i
	if (proc >= 99) found = true
      }
      else ord  = ord :+ i
      i += 1
    }
    println("i = " + i + " proc = " + proc + " nbouncy = " + nbouncy)
    //println(ord)
  }
}

