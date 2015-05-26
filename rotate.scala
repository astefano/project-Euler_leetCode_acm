/* https://code.google.com/codejam/contest/544101/dashboard#s=p0
*/

import scala.math._

object rotate {

  def printM(m: Array[Array[Int]]) = {
    val nr = m.length
    val nc = m(0).length
    val lr = (0 to nr-1)
    val lc = (0 to nc-1)
      for(i <- lr) {
	for(j <- lc) {
	  if (m(i)(j) == 0) print(".") else if (m(i)(j) == 1) print("R") else print("B")
	}
	println()
      }
  }

  def rotate(m: Array[Array[Int]]) = {
    val n = m.length
    val nm = Array.fill(n,n)(0)    
    val l = (0 to n-1)
    for(i <- l; j <- l) nm(i)(j) = m(n-1-j)(i)
    println("rotate:")
    printM(nm)
    nm
  }

  def gravity(m: Array[Array[Int]]) = {
    var aux = m.filter(mi => mi.sum > 0)
    val nr = aux.length
    if (nr == 0) aux
    else {
      val nc = aux(0).length
      //val nm = Array.fill(nr,nc)(0)    
      val lr = (0 to nr-2)
	val lc = (0 to nc-1)
	val lcr = (nc-1 to 0 by -1)
	val lrr = (nr-2 to 0 by -1)
	  for(i <- lrr; j <- lcr) {
	    var c = i + 1
	    if (aux(i)(j) != 0) {
	      while(c < nr && aux(c)(j) == 0)  c+=1
	      //if (c == 1) nm(c)(j) = aux(i)(j) else nm(c-1)(j) = aux(i)(j)
	      //nm(c-1)(j) = aux(i)(j)	      
	      if (c > i + 1) {
		aux(c-1)(j) = aux(i)(j)
		aux(i)(j) = 0
		//println(i + ", " + j + ":")
		//printM(aux)
	      }
	    }
	  }    
      //for(j <- lc) if (aux(nr-1)(j) != 0) nm(nr-1)(j) = aux(nr-1)(j)
      println("gravity:")
      printM(aux)     
      aux 
    }
  }

  def maxCS(l: Array[Int]) = {
    val n = l.length
    var i = 0
    var n1 = 0
    var n2 = 0
    var mn1 = 0
    var mn2 = 0
    while(i < n) {
      if (l(i) == 1) n1+=1
      else {
	if (mn1 < n1) mn1 = n1
	n1 = 0
      }
      if (l(i) == 2) n2+=1
      else {
	if (mn2 < n2) mn2 = n2
	n2 = 0
      }
      i+=1
    }
    if (mn1 < n1) mn1 = n1
    if (mn2 < n2) mn2 = n2
    if (mn1 < mn2) (2, mn2)
    else if (mn2 < mn1) (1, mn1)
    else (0, mn1)
  }

  def winner(m: Array[Array[Int]], k: Int) = {
    if (m.length == 0) "Neither"
    else {
      var n1c = 0
      var n1d = 0
      var n1cd = 0
      var n2c = 0
      var n2d = 0
      var n2cd = 0
      var mv = Map((1,0), (2,0), (0, 0))
      val n = m.length
      val indices = (0 to n-1)
	for(i <- indices) {
	  //parcurge linii
	  val (t, v) = maxCS(m(i))
	  //println(t + " " + v)
	  if (mv(t) < v) mv = mv.updated(t, v)
	  //coloane
	  if (mv(1) < n1c) mv = mv.updated(1, n1c)
	  if (mv(2) < n2c) mv = mv.updated(2, n2c)
	  n1c=0
	  n2c=0
	  for(j <- indices) {
	    if (m(j)(i) == 1) n1c+=1 
	    else {
	      if (mv(1) < n1c) mv = mv.updated(1, n1c)
	      n1c = 0
	    }
	    if (m(j)(i) == 2) n2c+=1
	    else {
	      if (mv(2) < n2c) mv = mv.updated(2, n2c)
	      n2c = 0
	    }
	  }
	  //diag 
	  if (mv(1) < n1d) mv = mv.updated(1, n1d)
	  if (mv(2) < n2d) mv = mv.updated(2, n2d)
	  n1d=0
	  n2d=0
	  for(j <- (i to 0 by -1)) {
	    if (m(j)(n-1-j) == 1) n1d+=1
	    else {
	      if (mv(1) < n1d) mv = mv.updated(1, n1d)
	      n1d = 0
	    }
	    if (m(j)(n-1-j) == 2) n2d+=1
	    else {
	      if (mv(2) < n2d) mv = mv.updated(2, n2d)
	      n2d = 0
	    }
	  }

	  //counterdiag
	  if (mv(1) < n1cd) mv = mv.updated(1, n1cd)
	  if (mv(2) < n2cd) mv = mv.updated(2, n2cd)
	  n1cd = 0
	  n2cd = 0
	  for(j <- (0 to n-1-i)) {
	    //println(i + " : " + (i+j, j))
	    if (m(i+j)(j) == 1) n1cd+=1
	    else {
	      if (mv(1) < n1cd) mv = mv.updated(1, n1cd)
	      n1cd = 0
	    }
	    if (m(i+j)(j) == 2) n2cd+=1
	    else {
	      if (mv(2) < n2cd) mv = mv.updated(2, n2cd)
	      n2cd = 0
	    }
	  }
	}

      if (mv(1) < n1c) mv = mv.updated(1, n1c)
      if (mv(2) < n2c) mv = mv.updated(2, n2c)
      if (mv(1) < n1d) mv = mv.updated(1, n1d)
      if (mv(2) < n2d) mv = mv.updated(2, n2d)
      if (mv(1) < n1cd) mv = mv.updated(1, n1cd)
      if (mv(2) < n2cd) mv = mv.updated(2, n2cd)

      println(mv)

      val mm = max(mv(0), max(mv(1), mv(2)))
      println("mm = " + mm)
      if (mm < k) "Neither"    
      else if (mv(0) > max(mv(1), mv(2)) || (mv(1) == mv(2) && mv(1) > mv(0)) || (mv(1) >= k && mv(2) >= k)) "Both" 
      else if (mv(1) > mv(2)) "Red"
      else "Blue"
    }
  }

 import scalax.io._
  import scalax.file.{ FileOps, Path, NotFileException }


  def main(args: Array[String]) {
    val start = System.currentTimeMillis

    //val file = "small-rot.in"   
    val file = "large-rot.in"   
    //val file = "test.in"   
    val rf: Input = Resource.fromFile(file)

    //val fileOut = "small-rot1.out"   
    val fileOut = "large-rot.out"   
    //val fileOut = "test.out"   
    val rfo: Output = Resource.fromFile(fileOut)

    val lines = rf.lines().filter(_!="")
    val nlines = lines.size
    var out = ""
    val ncases = lines(0).toInt
    var cp = 1
    (0 to ncases-1) foreach {
      i => 
	val nk = lines(cp).split(" ")
	val n = nk(0).toInt
	val k = nk(1).toInt
	cp += 1
	val m = Array.fill(n,n)(0)
	(0 to n-1) foreach {
	  l => 
	    lines(cp).zipWithIndex.filter(_._1 != '.').foreach {
	      ci =>  
		val c = ci._1
		val i = ci._2
		if (c == 'R') m(l)(i) = 1 else m(l)(i) = 2
	    }
	  cp += 1
	}
	
	if(i == 56) {
	  println("n = " + n + " k = " + k)
	  println("initial:")
	  printM(m)
	  out += "Case #"+(i+1)+": " + winner(gravity(rotate(m)),k) + "\n"
	}
	//out += "Case #"+(i+1)+": " + winner(gravity(rotate(m)),k) + "\n"
    }
    rfo.write(out)
    println( "Time (s): " + ( System.currentTimeMillis() - start ) / 1000. )
  }
}
