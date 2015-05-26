object P303 {


  import scalax.io._
  import scalax.file.{ FileOps, Path, NotFileException }

  def main(args: Array[String]) {
    val start = System.currentTimeMillis
    
    val file = "/media/ubuntuPart2/docs/topcoder/p303.txt"
    val rf: Input = Resource.fromFile(file)
    //val out = "/media/ubuntuPart2/docs/topcoder/p303Out.txt"
    //val rfO: Output = Resource.toFile(out)

    val lines = rf.lines().filter(_!="")
    val nlines = lines.size
    println("n lines = " + nlines)

    var s = BigInt(0)
    lines foreach {
      l => 
	val res = l.split(" ")	
	val i = res(0).toInt
	val v = BigInt(res(1))
	s += v
	if (i < 100) print(v.toInt*i + " ")
	//out.write(i + " " + BigInt(v)*i)
    }
    println("s = " + s)
    println( "Time (s): " + ( System.currentTimeMillis() - start ) / 1000. )
  }
} 


