object P13{

  import scalax.io._
  import scalax.file.{ FileOps, Path, NotFileException }

def main(args: Array[String]) {
  val start = System.currentTimeMillis
  
  val file = "/media/ubuntuPart2/docs/topcoder/inP13.txt"
  val rf: Input = Resource.fromFile(file)

  val lines = rf.lines().filter(_!="")
  val nlines = lines.size
  println("n lines = " + nlines)
  var clines = 0
  var s = BigInt(0)
  lines foreach {
    li => 
      clines = clines + 1
      s += BigInt(li)
  }

  println("sum = " + s + " first 10 = " + s.toString.take(10) )
  println( "Time (s): " + ( System.currentTimeMillis() - start ) / 1000. )
  }
} 
