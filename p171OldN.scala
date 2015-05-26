object P171 {

  import scalax.io._
  import scalax.file.{ FileOps, Path, NotFileException }


  val pp = List(1, 4, 9, 16, 25, 36, 49, 64, 81, 100, 121, 144, 169,
		196, 225, 256, 289, 324, 361, 400, 441, 484, 529, 576, 625, 676, 729,
		784, 841, 900, 961, 1024, 1089, 1156, 1225, 1296, 1369, 1444, 1521,
		1600)

  val lim = 20

  /**@returns the sum of the last 9 digits for each n of at most limnd digits which satisfy (C);
    * (C) is: parikh image of n = lapp
    */
  def sum9(limnd: Int, lapp: List[Int]) = {
    val limnzeros = limnd - lapp.sum
  }

  val indices = List.range(0, lim)

  val digits = List.range(0, 10)

  def upd(x: Int, m: Int, lim: Int, nap: Int) = if (x < m) (x, lim - nap, nap) else if (x == m) (0, lim, nap) else (0, lim, 0)  

  def f(r: scala.collection.mutable.MutableList[Int]) = digits.map{d => d*d*r(d)}.sum

  def getCombi(n: Int) = {
    var x = 0
    var faux = 0
    var c = 0

   var r = scala.collection.mutable.MutableList.fill(10)(0)

    val i2 = 9*9
    val nlim = math.min(lim - r.sum, (n-f(r))/i2) + 1
    List.range(0, nlim) foreach {
      d9 => 
	x = d9*i2
	r = scala.collection.mutable.MutableList.fill(10)(0)
	if (x <= n) {
	  r(9) = d9
	  if (x == n) {
	    if (c < 40) println("9r = " + r + " x = " + x)
	    x = 0
	    r = scala.collection.mutable.MutableList.fill(10)(0)
	    c += 1
	  }
	}
      val faux8 = f(r)
      val i82 = 8*8
      val nlim = math.min(lim - r.sum, (n-faux8)/i82) + 1
      List.range(0, nlim) foreach {
	d8 =>
	  val nx = faux8 + d8*i82
	  if(nx <= n) {
	    r(8) = d8
	    x = nx
	  if (x == n) {
	     if (c < 40) println("8r = " + r + " x = " + x)
	    x = 0
	    r = scala.collection.mutable.MutableList.fill(10)(0)
	    c+=1
	  }
	}
      val faux7 = f(r)
      val i72 = 7*7
      val nlim = math.min(lim - r.sum, (n-faux7)/i72) + 1
	List.range(0, nlim) foreach {
	  d7 =>
	    val nx = faux7 + d7*i72
	if (nx <= n) {
	  r(7) = d7
	  x = nx
	  if (x == n) {
	     if (c < 40) println("7r = " + r + " x = " + x)
	    x = 0
	  r = scala.collection.mutable.MutableList.fill(10)(0)
	    c += 1
	  }
	}
      val faux6 = f(r)
      val i62 = 6*6
      val nlim = math.min(lim - r.sum, (n-faux6)/i62) + 1
	  List.range(0, nlim) foreach {
	    d6 =>
	      val nx = faux6 + d6*i62
	if (nx <= n) {
	  r(6) = d6
	  x = nx
	  if (x == n) {
	     if (c < 40) println("6r = " + r + " x = " + x)
	    x = 0
	  r = scala.collection.mutable.MutableList.fill(10)(0)
	    c+=1
	  }
	}
      val faux5 = f(r)
      val i52 = 5*5
      val nlim = math.min(lim - r.sum, (n-faux5)/i52) + 1
   	    List.range(0, nlim) foreach {
	      d5 =>
		val nx = faux5 + d5*i52
	if (nx <= n) {
	  r(5) = d5
	  x = nx
	  if (x == n) {
 if (c < 40) 	    println("5r = " + r + " x = " + x)
	    x = 0
	  r = scala.collection.mutable.MutableList.fill(10)(0)
	    c+=1
	  }
	}
      val faux4 = f(r)
      val i42 = 4*4
      val nlim = math.min(lim - r.sum, (n-faux4)/i42) + 1
	      println("4nlim = " + nlim)
   	      List.range(0, nlim) foreach {
		d4 =>
		  val nx = faux4 + d4*i42
	if (nx <= n) {
	  r(4) = d4
	  x = nx
	  //if (d4 == 1) println("4: x = " + x)
	  if (x == n) {
 if (c < 40) 	    println("4r = " + r + " x = " + x)
	    x = 0
	  r = scala.collection.mutable.MutableList.fill(10)(0)
	    c+=1
	  }
	}
      val faux3 = f(r)
      val i32 = 3*3
      val nlim = math.min(lim - r.sum, (n-faux3)/i32) + 1
		println("3nlim = " + nlim)
   		List.range(0, nlim) foreach {
		  d3 =>
		    val nx = faux3 + d3*i32
	if (nx <= n) {
	  r(3) = d3
	  if (d4 ==1 && d3 == 1) println("3: faux = " + faux3 + " r = " + r)
	  x = nx
	  if (x == n) {
 if (c < 40) 	    println("3r = " + r + " x = " + x)
	    x = 0
	  r = scala.collection.mutable.MutableList.fill(10)(0)
	    c+=1
	  }
	}
      val faux2 = f(r)
      val i22 = 2*2
      val nlim = math.min(lim - r.sum, (n-faux2)/i22) + 1
   		  List.range(0, nlim) foreach {
		    d2 =>
		      val nx = faux2 + d2*i22
	if (nx <= n) {
	  r(2) = d2
	  x = nx
	  if (x == n) {
 if (c < 40) 	    println("2r = " + r + " x = " + x)
	    x = 0
	  r = scala.collection.mutable.MutableList.fill(10)(0)
	    c+=1
	  }
	}
      val faux1 = f(r)
      val nlim = math.min(lim - r.sum, (n-faux1))
   		    List.range(0, nlim) foreach {
		      d1 =>
			val nx = faux1 + d1
	if (nx == n) {
	  r(1) = d1
	  if (c < 40) 	    println("r = " + r + " x = " + x)
	    x = 0
	    r = scala.collection.mutable.MutableList.fill(10)(0)
	    c+=1
	}
 else {
	    x = 0
	    r = scala.collection.mutable.MutableList.fill(10)(0)
 }
		    }
		  }
		}
	      }
	    }
	  }
	}
      }
    }
  }

  val tenTo11 = ("1" + List.fill(11)(0).mkString).toLong

def main(args: Array[String]) {
  val start = System.currentTimeMillis

  getCombi(pp(4))
  
/*
  val file = "/media/ubuntuPart2/docs/topcoder/outP171.txt"
  val rf: Input = Resource.fromFile(file)

  val lines = rf.lines().filter(_!="")
  val nlines = lines.size
  println("n lines = " + nlines)
  var s = 0L
  var n = 0L
  lines foreach {
    li => 
      val l = li.split(" ").toList.zipWithIndex.map{x => List.fill(x._1.toInt)(x._2)}.filter(_!=List()).flatten
/*      val lp = l.permutations.toList.filter(el => el.head != 0)
      val m = if (lp.length > 5) 5 else lp.length
      val r = lp.foldLeft(0L)((r,c) => r + (c.mkString.takeRight(9).toLong))
      */ 
      val lp = l.permutations
      lp foreach {
	el => 	  
	  if (el.head != 0) {
	    n += 1
	    //if (n < 20) println("el = " + el)
	    val r = el.mkString.takeRight(11).toLong
	    s += r
	    if (s > tenTo11) s = s % tenTo11
	  }
      }
      println("l = " + l + " s = " + s + " n = " + n)
  }

  println("sum = " + s)
  */ 
  println( "Time (s): " + ( System.currentTimeMillis() - start ) / 1000. )
  }
} 
