object P99 {

  import scalax.io._
  import scalax.file.{ FileOps, Path, NotFileException }

  def pow(b: Int, exp: Int) = {
    var ini = BigInt(b)
    var i = 1
    while(i < exp) {
      i=2*i
      if (i < exp) ini = ini*ini      
    }
    i = i / 2
    //println("ini = " + ini + " i = " + i)
    while(i < exp) {
      ini = ini*b
      i+=1
    }
    ini
  }

  def main(args: Array[String]) {
    val start = System.currentTimeMillis

    val file = "/media/ubuntuPart2/docs/topcoder/inP99.txt"   
    val rf: Input = Resource.fromFile(file)
    val lines = rf.lines().filter(_!="")
    val nlines = lines.size
    println("n lines = " + nlines)
    val pairs = lines.toList.map{li => li.trim.split(",").filter(_!="").map{_.toInt}}.map{x => (x(0), x(1))}
    var max = BigInt(0)
    var cline = 0
    var cp = pairs(1)
    List.range(0,1000) foreach {    	  
      p => 
	val b = pairs(p)._1
	val exp = pairs(p)._2
	//if (cp._1 / b)
	val bexp = pow(b,exp)
      if (bexp > max) {
	max = bexp
	cline = p
	cp = pairs(p)
      }
    }
    println("cline = " + cline + " cp = " + cp)
    println( "Time (s): " + ( System.currentTimeMillis() - start ) / 1000. )
  }
}
