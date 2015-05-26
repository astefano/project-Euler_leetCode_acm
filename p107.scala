object P107 {

  import scalax.io._
  import scalax.file.{ FileOps, Path, NotFileException }

  def prim(m: List[(Int, Int, Int)], size: Int) = {
    var vn = Set(0)  
    var en = Set[(Int, Int)]()
    var s = 0
    while (vn.size < size) {
      //find all m(u)(v) with u in vn 
      val cands = m.filter(el => (vn(el._1) && !vn(el._2)) || (vn(el._2) && !vn(el._1))).sortBy(_._3)
      //println("cands = " + cands + " vn = " + vn + " en = " +  en)
      val minc = cands.head
      if (vn(minc._1)) vn +=  minc._2 
      else vn += minc._1
      en += ((minc._1, minc._2))
      s += minc._3
    }
    //println("en = " + en)
    println(" s = " + s)
  }

  def main(args: Array[String]) {
  val start = System.currentTimeMillis
  val file = "/media/ubuntuPart2/docs/topcoder/p107.txt"
  val rf: Input = Resource.fromFile(file)
  val lines = rf.lines().filter(_!="")
  val n = lines.size
  println("n lines = " + n)
  var i = 0
  val mat = lines.toArray.map(x => x.split(",").map(x => if (x == "-") 0 else x.toInt))    
  //transf mat to an adjacency list  
  val indices = (0 to n-1)
  val adjL = for(i <- indices; j <- (i+1 to n-1); if (mat(i)(j) != 0)) yield ((i, j, mat(i)(j)))
  //println("adjL = " + adjL + " adjL.sum = " + adjL.map{_._3}.sum)
  println("adjL.sum = " + adjL.map{_._3}.sum)
  prim(adjL.toList, n)
  //println("adjL = " + mat(6)(6))
  println( "Time (s): " + ( System.currentTimeMillis() - start ) / 1000. )
  }
}
