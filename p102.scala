object P102 {

  def areCollinear(x1: Int, y1: Int, x2: Int, y2: Int, x3: Int, y3: Int) = x1*(y2-y3)+x2*(y3-y1)+x3*(y1-y2) == 0

  def insideTriangle(x1: Int, y1: Int, x2: Int, y2: Int, x3: Int, y3: Int, x: Int, y: Int) = {
    val l1 = 1.*( (y2-y3)*(x-x3) + (x3-x2)*(y-y3) ) / ( (y2-y3)*(x1-x3) + (x3-x2)*(y1-y3) )
    if (0. < l1 && l1 < 1.) {
      val l2 = 1.*( (y3-y1)*(x-x3) + (x1-x3)*(y-y3) ) / ( (y2-y3)*(x1-x3) + (x3-x2)*(y1-y3) )
      if (0. < l2 && l2 < 1.) {	
	val l3 = 1. - l1 - l2
	(0. < l3 && l3 < 1.) 
      }
      else false
    }
    else false
  }

  import scalax.io._
  import scalax.file.{ FileOps, Path, NotFileException }

  def main(args: Array[String]) {
    val start = System.currentTimeMillis
    
    val file = "/media/ubuntuPart2/docs/topcoder/triangles.txt"
    val rf: Input = Resource.fromFile(file)

    val lines = rf.lines().filter(_!="")
    val nlines = lines.size
    var n = 0
    var cl = 0
    println("n lines = " + nlines)
    lines foreach {
      li => 
	 cl += 1
      	 val l = li.split(",").toList.map{_.trim.toInt}
	 val x1 = l(0)
	 val y1 = l(1)
	 val x2 = l(2)
	 val y2 = l(3)
	 val x3 = l(4)
	 val y3 = l(5)
	 if (areCollinear(x1,y1,x2,y2,x3,y3)) println("the points in l" + l + " are collinear")
	 if (insideTriangle(x1,y1,x2,y2,x3,y3,0,0)) {
	   if (cl < 2) println("0 is inside triangle " + l)
	   n+=1
	 }
    }
    println("n=" + n)
    println( "Time (s): " + ( System.currentTimeMillis() - start ) / 1000. )
  }
} 


