object P96 {

  val allS = Set(1,2,3,4,5,6,7,8,9) 

  //def validL(l: List[Int]) = l.toSet == allS
  def validL(l: Array[Int]) = l.toSet == allS

  //def validM(m: List[List[Int]]) = List.range(0,9).foldLeft(true)((r,c) => r && validL(m(c)) && validL(col(m, c)))
  def validM(m: Array[Array[Int]]) = List.range(0,9).foldLeft(true)((r,c) => r && validL(m(c)) && validL(col(m, c)))

  //def col(m: List[List[Int]], j: Int) = List.range(0, m.length).map{i => m(i)(j)}
  def col(m: Array[Array[Int]], j: Int) = List.range(0, m.length).map{i => m(i)(j)}.toArray

  //def neighSq(m: List[List[Int]], i: Int, j: Int) = for(ii <- List.range(0,3); jj <- List.range(0,3)) yield m(3*(i/3) + ii)(3*(j/3) + jj)
  def neighSq(m: Array[Array[Int]], i: Int, j: Int) = for(ii <- List.range(0,3); jj <- List.range(0,3)) yield m(3*(i/3) + ii)(3*(j/3) + jj)

  import scala.collection.mutable.Stack

  def solve(m: Array[Array[Int]]): Array[Array[Int]] = { 
    val tovisit = Stack[(Int, Int, List[Int])]()
    def solveAux(m: Array[Array[Int]]): Array[Array[Int]] = {
      val rl = List.range(0,9).dropWhile{i => m(i).find(_==0)==None}
      if (rl == List()) m
      else {
	val r = rl.head
	val c = m(r).indexOf(0)
	val sols = (allS -- (m(r).filter{_>0}.toSet ++ col(m, c).filter{_>0}.toSet ++ neighSq(m, r, c).filter{_>0}.toSet)).toList
	if (sols == List()) {
	  var l = tovisit.pop
	  while(l._3 == List()) { 
	    m(l._1)(l._2) = 0
	    l = tovisit.pop
	  }	  
	  tovisit.push((l._1, l._2, l._3.tail))
	  m(l._1)(l._2) = l._3.head
	  solveAux(m)
	}
	else {
	  tovisit.push((r, c, sols.tail))	
	  m(r)(c)=sols.head
	  solveAux(m)  
	}
      }
    }
    solveAux(m)
  }

  import scalax.io._
  import scalax.file.{ FileOps, Path, NotFileException }

  def main(args: Array[String]) {
    val start = System.currentTimeMillis
    
    val file = "/media/ubuntuPart2/docs/topcoder/sudoku.txt"
    val rf: Input = Resource.fromFile(file)
    val lines = rf.lines().filter(_!="").toList
    var s = 0 
    List.range(0,2) foreach {
      i => 	
	val m = List.range(1,10).map{j => lines(10*i+j).map{_.toInt - '0'}.toList}
	val ma = m.map{_.toArray}.toArray
	val mr = solve(ma)
	s += mr(0)(0)*100 + mr(0)(1)*10 + mr(0)(2)
	if (i == 1) println(s)
	if (i == 0) println(mr.map{_.toList}.toList.foldLeft("")(_+"\n"+_))
    }
    println("s = " + s)
    println( "Time (s): " + ( System.currentTimeMillis() - start ) / 1000. )
  }
}
