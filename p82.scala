object P82 {

  import scalax.io._
  import scalax.file.{ FileOps, Path, NotFileException }

  def main(args: Array[String]) {
  val start = System.currentTimeMillis
  val file = "/media/ubuntuPart2/docs/topcoder/matrixP82.txt"
  val rf: Input = Resource.fromFile(file)
  val lines = rf.lines().filter(_!="")
  val n = lines.size
  println("n lines = " + n)
  var i = 0
  val mat = lines.toArray.map(x => x.split(",").map(_.toInt))    
  val s = Array.ofDim[Long](n, n)
  List.range(0,n) foreach {
    i => 
      s(i)(0) = mat(i)(0)
  }
  List.range(1,n) foreach {
    col => 
      s(0)(col) = List(s(0)(col-1), s(1)(col-1)+mat(1)(col)).min + mat(0)(col)
      s(n-1)(col) = List(s(n-1)(col-1), s(n-2)(col-1)+mat(n-2)(col)).min + mat(n-1)(col)
      (n-2 to 1 by -1) foreach {
	row => 
	  s(row)(col) = List(s(row)(col-1), s(row+1)(col-1)+mat(row+1)(col), s(row-1)(col-1)+mat(row-1)(col), s(row+1)(col)).min + mat(row)(col)
      }
      List.range(1,n-1) foreach {
	row => 
	  s(row)(col) = math.min(s(row)(col), List(s(row)(col-1), s(row+1)(col-1)+mat(row+1)(col), s(row-1)(col-1)+mat(row-1)(col), s(row-1)(col)).min + mat(row)(col))
      }
  }

  //println("mat = " + mat.toList.foldLeft("")(_+"\n"+_.toList))
  //println("res = " + s.toList.foldLeft("")(_+"\n"+_.toList))
  val lastcol = List.range(0,n).map(j => s(j)(n-1))
  println("last col = " + lastcol + " with min = " + lastcol.min  )
  println( "Time (s): " + ( System.currentTimeMillis() - start ) / 1000. )
  }
}
