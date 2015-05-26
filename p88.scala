object P88 {

  def findNNaive(k: Int) = {
    var i = 2
    var sols = Set(4)
    //var all = Set((1,1,1))
    var all = List((List(1),1,1))
    //at i we have all the vectors of dim i which have eq sum and prod.
    while(i < k) {
      var temp = List[(List[Int],Int,Int)]()
      //var temp = Set[(Int,Int,Int)]()
      var min = i+all.last._2
      all foreach {
	c => 
	  val l = c._1.last
	  val s = c._2
	  val p = c._3
	  List.range(l, k+1).takeWhile {
	    el => 
	      val ns = s+el
	      val np = p*el	     
	      if (ns == np & min > ns) min = ns
	      val stop = (ns < np)
	       //println("el = " + el + " s = " + s + " p = " + p + " stop = " + stop)
	      if (!stop) temp = temp :+ ((c._1:+el, ns, np))
	      !stop
	  }
      }
      all = temp
      sols += min
      println("at i = " + i + " all.length = " + all.length +  " sols = " + all.filter(x => x._2 == x._3 && x._2 == min).map(x => x._1.filter(_!=1)) + " min = " + min)
      i+=1
    }
    println("sol = " + (sols.foldLeft(0)(_+_)-2))
    sols
  } 

  import scala.collection.mutable.Stack

  //the number of 1s in a[0..n] is bigger than i0
  def findN(k: Int, i0: Int) = {
    var i = i0
    var imin = 0
    var min = k*k
    var found = false
    var stop = false
    while (i < k && !found) {
      var psol = new Stack[(Int,Int,Int)]
      //psol.push((1, i, 1))
      psol.push((2, 2+i, 2))
      var j = i + 1
      var npops = 0
      var l = 2
      var cs = 2+i
      var cp = 2
      //while(!found && psol != Stack()) {
      while (psol.length > 0 && psol.last._1 <= k/2+1) {
	List.range(0,npops-1) foreach {
	  p => 
	    psol.pop
	}
	if (npops > 0 && psol.length > 0) {	  
	  val lsol = psol.pop	  
	  l = lsol._1 + 1
	  cs = lsol._2 - lsol._1
	  cp = lsol._3 / lsol._1
	  j = i + psol.length
	  //println("after popping " + npops + " j = " + j + " cs = " + cs + " cp = " + cp)
	  npops = 0
	}
	  if (l > k) stop = true
	  else stop = false
	  //println("before while(j): at i = " + i + " j = " + j + " sol = " + psol + " cs = " + cs  + " cp = " + cp + " npops = " + npops + " stop = " + stop)
	  while(j < k && !stop) {
	    cs += l
	    cp *= l
	    if (cs < cp) stop = true
	    else psol.push((l,cs,cp))
	    j += 1
	  }
	if (cs == cp) {
	  //found = true
	  if (min > cs) {
	    min = cs
	    imin = i 
	  }
	  //println("at i = " + i  + " FOUND sol = " + psol)
	}
	//else if (cs < cp) npops = (npops + 1) 
	//else 
	npops = (npops + 1) 
	if (npops > psol.length) npops = 0
	//println("after while(j): at i = " + i + " sol = " + psol + " cs = " + cs  + " cp = " + cp + " npops = " + npops)
      }
      i += 1
    }        
    println("k = " + k + " i0 = " + i0 + " imin = " + imin + " min = " + min)
    min
  }

  import scalax.io._
  import scalax.file.{ FileOps, Path, NotFileException }

  def main(args: Array[String]) {
    val start = System.currentTimeMillis
    val k = args(0).toInt
    //val i = args(1).toInt
    val file = "/media/ubuntuPart2/docs/topcoder/outP88I.txt"
    val rf: Input = Resource.fromFile(file)
    val lines = rf.lines().filter(_!="")
    val n = lines.size
    println("n lines = " + n)
    val li = lines.toList.map(x => x.split(" ").map(_.toInt)).flatten           
//    println(li(5))
//    findN(k,li(k))
/**/    
    //val sols1 = findNNaive(k+1)	
    val sols2 = List.range(3, k+1).map{k => findN(k, li(k))}
    println("sol = " + sols2.toSet.foldLeft(4)(_+_))
    //List.range(2,101)
//    println("sol1 - sol2 " + (sols1 -- sols2.toSet))
//    println("sol2 - sol1 " + (sols2.toSet -- sols1))
    
    println( "Time (s): " + ( System.currentTimeMillis() - start ) / 1000. )
  }

}
