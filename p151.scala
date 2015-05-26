object P151 {

  def f(s: String) = s match {
    case "a5" => Set()
    case "a4" => Set("a5")
    case "a3" => Set("a4","a5")
    case "a2" => Set("a3","a4","a5")
    case "a1" => Set("a2","a3","a4","a5")
  }

  def upd(s: List[String], i: Int) = {
    //println(" replace s(" + i + ") = " + s(i))
    //println(s)
    val res = s.take(i) ++ f(s(i)) ++ s.drop(i+1)
    //println(res + "\n")
    res
  }

  import scala.collection.mutable.Stack

  def main(args: Array[String]) = {
    val lim = args(0).toInt

    val start = System.currentTimeMillis       

    var expv = -2.
    //the triples are (the content of the envelope, the probability to reach this content, the level)
    val tovisit = new Stack[(List[String], Double, Int)] 
    tovisit.push((List("a1"), 1, 1))   
    while(tovisit!=Nil) {// && i < reachLim) {
      val e = tovisit.pop
      //println(e._1)
      val le = e._1.length
      expv += e._2/le
      if (e._3 < lim) {
	(0 to le-1) foreach {
	  i => 
	    tovisit.push((upd(e._1, i), e._2/le, e._3+1))
	}
      }
    }
    println("expv = " + expv)

    println( "Time (s): " + ( System.currentTimeMillis() - start ) / 1000. )

  }


}
