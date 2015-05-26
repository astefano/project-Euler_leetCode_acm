  val file = "/media/ubuntuPart2/docs/topcoder/primes1.txt"
  val rf: Input = Resource.fromFile(file)
  val lines = rf.lines().filter(_!="")  
  lines foreach {
    pli => 
      val cprimes = pli.trim.split(" ").filter(_!="").map(_.toInt)
      cprimes foreach {
	p => 
	val lp = p.toString.length
	if (p.toString.map{c=> (c-'0').toInt}.toSet == List.range(1,lp+1).toSet) println(p)	
      }
  }

