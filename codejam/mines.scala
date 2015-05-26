/* https://code.google.com/codejam/contest/2974486/dashboard#s=p2
*/
object digits {

  import scalax.io._
  import scalax.file.{ FileOps, Path, NotFileException }

  def printM(m: Array[Array[Int]], cc: Int, cr: Int) = {
    val r = m.length
    val c = m(0).length
    val s = for {
      i <- (0 to r-1)
      j <- (0 to c-1)
      val cij = (if (i == cc && j == cr) "c" else if (m(i)(j) == -1) "*" else ".") + (if(j == c-1) "\n" else "")
    } yield cij
    println(s.mkString.trim)
  }

  def isMineInt(m: Array[Array[Int]], i: Int, j: Int) = if (m(i)(j) == -1) 1 else 0  

  //def isMine(m: Array[Array[Int]], i: Int, j: Int) = (m(i)(j) == -1) 
  def isMine(v: Int) = (v == -1) 

  def isMineV(v: Int) = if (v == -1) 1 else 0

  //counts mines around m(i)(j)
    def checkInside(m: Array[Array[Int]], i: Int, j: Int) = {
      if (isMine(m(i)(j))) true
      else {
	var n = 0 
	(i-1 to i+1) foreach {
	  k => 
	    (j-1 to j+1) foreach {
	      l => 
		if (m(k)(l) == -1) n += 1
	    }
	}
	m(i)(j) == n
      }
    }

  def isMineIntl(m: List[List[Int]], i: Int, j: Int) = if (m(i)(j) == -1) 1 else 0  

  def checkCornerUL(m: Array[Array[Int]], r: Int, c: Int) = (isMine(m(r)(c)) || (m(r)(c) == isMineInt(m, r, c+1) + isMineInt(m, r+1, c+1) + isMineInt(m, r+1, c)))

  def checkCornerUR(m: Array[Array[Int]], r: Int, c: Int) = (isMine(m(r)(c)) || (m(r)(c) == isMineInt(m, r, c-1) + isMineInt(m, r+1, c-1) + isMineInt(m, r+1, c)))

  def checkCornerDL(m: Array[Array[Int]], r: Int, c: Int) = (isMine(m(r)(c)) || (m(r)(c) == isMineInt(m, r, c+1) + isMineInt(m, r-1, c+1) + isMineInt(m, r-1, c)))

  def checkCornerDR(m: Array[Array[Int]], r: Int, c: Int) = (isMine(m(r)(c)) || (m(r)(c) == isMineInt(m, r, c-1) + isMineInt(m, r-1, c-1) + isMineInt(m, r-1, c)))

  def checkBordersHU(m: Array[Array[Int]], r: Int, s: Int, f: Int) = (s to f).foldLeft(true)((res, x) => res && (if (isMine(m(r)(x))) true else m(r)(x) == isMineInt(m, r, x-1) + isMineInt(m, r, x+1) + isMineInt(m, r+1, x-1) + isMineInt(m, r+1, x) + isMineInt(m, r+1, x+1)))
  
  def checkBordersHD(m: Array[Array[Int]], r: Int, s: Int, f: Int) = (s to f).foldLeft(true)((res, x) => res && (if (isMine(m(r)(x))) true else m(r)(x) == isMineInt(m, r, x-1) + isMineInt(m, r, x+1) + isMineInt(m, r-1, x-1) + isMineInt(m, r-1, x) + isMineInt(m, r-1, x+1)))

  def checkBordersVL(m: Array[Array[Int]], c: Int, s: Int, f: Int) = (s to f).foldLeft(true)((res, x) => res && (if (isMine(m(x)(c))) true else m(x)(c) == isMineInt(m, x-1, c) + isMineInt(m, x+1, c) + isMineInt(m, x-1, c+1) + isMineInt(m, x, c+1) + isMineInt(m, x+1, c+1))) 

  def checkBordersVR(m: Array[Array[Int]], c: Int, s: Int, f: Int) = (s to f).foldLeft(true)((res, x) => res && (if (isMine(m(x)(c))) true else m(x)(c) == isMineInt(m, x-1, c) + isMineInt(m, x+1, c) + isMineInt(m, x-1, c-1) + isMineInt(m, x, c-1) + isMineInt(m, x+1, c-1)))

  def check(m: Array[Array[Int]]) = {
    val r = m.length
    val c = m(0).length
    val res = for {
      i <- (1 to r-2)
      j <- (1 to c-2)
      if (checkInside(m, i, j))
	} yield true      
    res.foldLeft(true)(_ && _) && 
    checkCornerUL(m, 0, 0) && checkCornerUR(m, 0, c-1) && checkCornerDL(m, r-1, 0) && checkCornerDR(m, r-1, c-1) && 
    checkBordersHU(m, 0, 1, c-1) && checkBordersHD(m, r-1, 1, c-1) && 
    checkBordersVL(m, 0, 1, r-1) && checkBordersVR(m, c-1, 1, r-1) 
  }

  def printMl(m: List[List[Int]], cc: Int, cr: Int) = {
    val r = m.length
    val c = m(0).length
    val s = for {
      i <- (0 to r-1)
      j <- (0 to c-1)
      val cij = (if (i == cc && j == cr) "c" else if (m(i)(j) == -1) "*" else ".") + (if(j == c-1) "\n" else "")
    } yield cij
    println(s.mkString.trim)
  }
 
    def checkInsidel(m: List[List[Int]], i: Int, j: Int) = {
      if (isMine(m(i)(j))) true
      else {
	var n = 0 
	(i-1 to i+1) foreach {
	  k => 
	    (j-1 to j+1) foreach {
	      l => 
		if (m(k)(l) == -1) n += 1
	    }
	}
	m(i)(j) == n
      }
    }

  def checkCornerlUL(m: List[List[Int]], r: Int, c: Int) = (isMine(m(r)(c)) || (m(r)(c) == isMineIntl(m, r, c+1) + isMineIntl(m, r+1, c+1) + isMineIntl(m, r+1, c)))

  def checkCornerlUR(m: List[List[Int]], r: Int, c: Int) = (isMine(m(r)(c)) || (m(r)(c) == isMineIntl(m, r, c-1) + isMineIntl(m, r+1, c-1) + isMineIntl(m, r+1, c)))

  def checkCornerlDL(m: List[List[Int]], r: Int, c: Int) = (isMine(m(r)(c)) || (m(r)(c) == isMineIntl(m, r, c+1) + isMineIntl(m, r-1, c+1) + isMineIntl(m, r-1, c)))

  def checkCornerlDR(m: List[List[Int]], r: Int, c: Int) = (isMine(m(r)(c)) || (m(r)(c) == isMineIntl(m, r, c-1) + isMineIntl(m, r-1, c-1) + isMineIntl(m, r-1, c)))

  def checkBorderslHU(m: List[List[Int]], r: Int, s: Int, f: Int) = (s to f).foldLeft(true)((res, x) => res && (if (isMine(m(r)(x))) true else m(r)(x) == isMineIntl(m, r, x-1) + isMineIntl(m, r, x+1) + isMineIntl(m, r+1, x-1) + isMineIntl(m, r+1, x) + isMineIntl(m, r+1, x+1)))
  
  def checkBorderslHD(m: List[List[Int]], r: Int, s: Int, f: Int) = (s to f).foldLeft(true)((res, x) => res && (if (isMine(m(r)(x))) true else m(r)(x) == isMineIntl(m, r, x-1) + isMineIntl(m, r, x+1) + isMineIntl(m, r-1, x-1) + isMineIntl(m, r-1, x) + isMineIntl(m, r-1, x+1)))

  def checkBorderslVL(m: List[List[Int]], c: Int, s: Int, f: Int) = (s to f).foldLeft(true)((res, x) => res && (if (isMine(m(x)(c))) true else m(x)(c) == isMineIntl(m, x-1, c) + isMineIntl(m, x+1, c) + isMineIntl(m, x-1, c+1) + isMineIntl(m, x, c+1) + isMineIntl(m, x+1, c+1))) 

  def checkBorderslVR(m: List[List[Int]], c: Int, s: Int, f: Int) = (s to f).foldLeft(true)((res, x) => res && (if (isMine(m(x)(c))) true else m(x)(c) == isMineIntl(m, x-1, c) + isMineIntl(m, x+1, c) + isMineIntl(m, x-1, c-1) + isMineIntl(m, x, c-1) + isMineIntl(m, x+1, c-1)))

  def checkL(m: List[List[Int]]) = {
    if (m.isEmpty) true 
    else {
      val r = m.length
      val c = m(0).length
      if (r < 2 || c < 2) true 
      else {
	val res0 = for {
	  i <- (1 to r-2)
	  j <- (1 to c-2)
	  if (checkInsidel(m, i, j))
	} yield true      
	val res = res0.foldLeft(true)(_ && _) && 
	checkCornerlUL(m, 0, 0) && checkCornerlUR(m, 0, c-1) && checkCornerlDL(m, r-1, 0) && checkCornerlDR(m, r-1, c-1) && 
	checkBorderslHU(m, 0, 1, c-2) && checkBorderslHD(m, r-1, 1, c-2) && 
	checkBorderslVL(m, 0, 1, r-2) && checkBorderslVR(m, c-1, 1, r-2) 
	println("checking m = " + m + " -> " + res)
	res
      }
    }
  }

  def minesNeighbours(sol: List[Int], c: Int) = {
    val n = sol.length
    /*          ___                __            __ 
     * (middle)  _x  or  (start)   x   or (end)  _x
     */
    val nr = n/c
    val pInLastCol = n - c*nr
    //println("p = " + pInLastCol + " nr = " + nr + " n = " + n + " sol = " + sol)
    //case (end)
    if (n == nr * c) {
      //println("end:" + sol(n-2) + " " + sol(n-c-1) + " " + sol(n-c))
      isMineV(sol(n-2)) + isMineV(sol(n-c-1)) + isMineV(sol(n-c))     
    }
    //case (start)
    else if (pInLastCol == 1) {
      //println("start:" + sol(n-c-1) + " " + sol(n-c))
      isMineV(sol(n-c-1)) + isMineV(sol(n-c))
    }
    //case (middle)
    else {
      //println("middle:" + sol(n-2) + " " + sol(n-c-1) + " " + sol(n-c) + " " + sol(n-c+1))
      isMineV(sol(n-2)) + isMineV(sol(n-c-1)) + isMineV(sol(n-c)) + isMineV(sol(n-c+1))
    }
  }									     

  def checkEl(el: Int, sol: List[Int], c: Int) = {
    if (el == -1 || sol.length < c+1) true
    else {
      el == minesNeighbours(sol, c)
    }
  }											     
											     
  def gen(r: Int, c: Int, m: Int) = {
    //def genR(emptyPlaces: List[(Int, Int, Int)]) : List[List[(Int, Int, Int)]] = {
    def genR(emptyPlaces: List[Int]) : List[List[Int]] = {
      if (emptyPlaces.isEmpty) List(List())
      else {
        for {
	  sol <- genR(emptyPlaces.init)
	  v <- -1 to m
	  val n = sol.length
	  val currR = math.round(n/(1.*c))
	  val currC = n % c
	  //val nplace = (currR, currC, v)
	  val nExistingMines = sol.filter(x => x == -1).length 
	  //if ((currR + currC == r + c && nExistingMines == m) || nExistingMines <= m) && checkEl(v, sol, c))
	  val solL = sol.take(n-currC).grouped(c).toList
	  if (nExistingMines <= m && checkEl(v, sol, c) && checkL(solL))
	} yield v :: sol //yield nplace :: sol	
      } 
    }
    val init = for(i <- (0 to r-1); j <- (0 to c-1)) yield 0	     
    val res0 = genR(init.toList)
    println("res0 = " + res0)
    val res = res0.filter(l => l.filter(_ == -1).length == m).filter(l => checkL(l.grouped(c).toList))
    println("res = " + res)
    if (res.isEmpty) "Impossible"
    else {
      val sol = res(0).grouped(c).toList
      var cont = true
      var start = (0, 0)
      (0 to r-1).dropWhile {
	i => 
	  (0 to c-1).dropWhile {
	    j => 
	      if (sol(i)(j) != -1) {
		cont = false
		start = (i, j)
	      }
	      (sol(i)(j) == -1)
	  }
	cont
      }
      printMl(sol, start._1, start._2)
    }
  }

  def main(args: Array[String]) {
    val start = System.currentTimeMillis

    val file = "C-small.in"   
    //val file = "C-large-practice.in"   
    val rf: Input = Resource.fromFile(file)

    val fileOut = "C-small-practice.out"   
    //val fileOut = "C-large-practice.out"   
    val rfo: Output = Resource.fromFile(fileOut)

    val lines = rf.lines().filter(_!="")
    val nlines = lines.size
    var out = ""

    val ncases = lines(0).toInt
    (0 to ncases-1) foreach {
      i => 
	val ns = lines(i+1).split(" ")
	val r = ns(0).toInt
	val c = ns(1).toInt
	val m = ns(2).toInt
	println("r = " + r + " c = " + c + " m = " + m)
	out += "Case #"+(i+1)+": " + gen(r,c,m) + "\n"
    }
		  
    //println(out)
    rfo.write(out)
    println( "Time (s): " + ( System.currentTimeMillis() - start ) / 1000. )
  }

}
