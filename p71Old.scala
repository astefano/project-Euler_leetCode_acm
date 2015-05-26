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

  def upd(x: Int, m: Int, lim: Int, nap: Int) = if (x < m) (x, lim - nap, nap) else if (x == m) (0, lim, nap) else (0, lim, 0)  

  def getCombi(n: Int) = {
    var x = 0
/* 
    var r9 = 0
    var r8 = 0
    var r7 = 0
    var r6 = 0
    var r5 = 0
    var r4 = 0
    var r3 = 0
    var r2 = 0
    var r1 = 0
*/
    var c = 0

   var r = scala.collection.mutable.MutableList.fill(10)(0)
/*
    List.range(1,10) foreach {
      d => 
      List.range(0, nlim) foreach {
	napp =>
	  x += napp*d*d
	  val res = upd(x, n, nlim, napp)
	  //println("d = " + d + " res =" + res )
    	  x = res._1
	  nlim = res._2
	  r(d) = res._3
	  if (x == n) println("d = " + d + " napp = " + napp + "r = " + r)
      }
    }
  }
*/    

    indices foreach {
      d9 => 
	x = d9*9*9
	r = scala.collection.mutable.MutableList.fill(10)(0)
	if (x <= n) {
	  r(9) = d9
	  if (x == n) {
	    if (c < 20) println("r = " + r + " x = " + x)
	    x = 0
	    r = scala.collection.mutable.MutableList.fill(10)(0)
	    c += 1
	  }
	}
      val nlim = lim - r.sum
      List.range(0, nlim) foreach {
	d8 =>
	  val nx = x + d8*8*8
	  if(nx <= n) {
	    r(8) = d8
	    x = nx
	  if (x == n) {
	     if (c < 20) println("r = " + r + " x = " + x)
	    x = 0
	    r = scala.collection.mutable.MutableList.fill(10)(0)
	    c+=1
	  }
	}
	val nlim = lim - r.sum
	List.range(0, nlim) foreach {
	  d7 =>
	    val nx = x + d7*7*7
	if (nx <= n) {
	  r(7) = d7
	  x = nx
	  if (x == n) {
	     if (c < 20) println("r = " + r + " x = " + x)
	    x = 0
	  r = scala.collection.mutable.MutableList.fill(10)(0)
	    c += 1
	  }
	}
	  val nlim = lim - r.sum
	  List.range(0, nlim) foreach {
	    d6 =>
	      val nx = x + d6*6*6
	if (nx <= n) {
	  r(6) = d6
	  x = nx
	  if (x == n) {
	     if (c < 20) println("r = " + r + " x = " + x)
	    x = 0
	  r = scala.collection.mutable.MutableList.fill(10)(0)
	    c+=1
	  }
	}
	    val nlim = lim - r.sum
   	    List.range(0, nlim) foreach {
	      d5 =>
		val nx = x + d5*5*5
	if (nx <= n) {
	  r(5) = d5
	  x = nx
	  if (x == n) {
 if (c < 20) 	    println("r = " + r + " x = " + x)
	    x = 0
	  r = scala.collection.mutable.MutableList.fill(10)(0)
	    c+=1
	  }
	}
	      val nlim = lim - r.sum
   	      List.range(0, nlim) foreach {
		d4 =>
		  val nx = x + d4*4*4
	if (nx <= n) {
	  r(4) = d4
	  x = nx
	  //if (d4 == 1) println("4: x = " + x)
	  if (x == n) {
 if (c < 20) 	    println("r = " + r + " x = " + x)
	    x = 0
	  r = scala.collection.mutable.MutableList.fill(10)(0)
	    c+=1
	  }
	}
		val nlim = lim - r.sum
   		List.range(0, nlim) foreach {
		  d3 =>
		    val nx = x + d3*3*3
		    //if (r(4) ==1 && d3 == 1) println("3: x = " + x)
	if (nx <= n) {
	  r(3) = d3
	  x = nx
	  if (x == n) {
 if (c < 20) 	    println("r = " + r + " x = " + x)
	    x = 0
	  r = scala.collection.mutable.MutableList.fill(10)(0)
	    c+=1
	  }
	}
		  val nlim = lim - r.sum
   		  List.range(0, nlim) foreach {
		    d2 =>
		      val nx = x + d2*2*2
	if (nx <= n) {
	  r(2) = d2
	  x = nx
	  if (x == n) {
 if (c < 20) 	    println("r = " + r + " x = " + x)
	    x = 0
	  r = scala.collection.mutable.MutableList.fill(10)(0)
	    c+=1
	  }
	}
		    val nlim = lim - r.sum
   		    List.range(0, nlim) foreach {
		      d1 =>
			val nx = x + d1
	if (nx <= n) {
	  r(1) = d1
	  x = nx
	  if (x == n) {
 if (c < 20) 	    println("r = " + r + " x = " + x)
	    x = 0
	    r = scala.collection.mutable.MutableList.fill(10)(0)
	    c+=1
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
