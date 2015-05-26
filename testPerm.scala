object Perm {

  def f(pi: List[Int]) = {
    val n = pi.length
    List.range(0, n) map { i => pi.indexOf(i) } 
  }

  def g(pi: List[Int]) = {
    val n = pi.length
    List.range(0, n) map { i => n - 1 - pi(i) }
  }

  def h(pi: List[Int]) = pi.reverse

  def m(pi: List[Int]) = ...

  def mprint(pi: List[Int]) = pi map {el => el + 1}

  import scala.collection.mutable._

  def main(args: Array[String]) {
    val n = args(0).toInt

    val start = System.currentTimeMillis

    val perms = List.range(0, n).permutations
    perms foreach {
      pi => 
	println("start from pi = " + mprint(pi))
      //var tovisit = new Stack[(String, List[Int])]
      var tovisit = new Queue[(String, List[Int])]
      //tovisit.push(("", pi))
      tovisit.enqueue(("", pi))
      var visited = Set[List[Int]]()
      while(!tovisit.isEmpty) {
	//val (path, npi) = tovisit.pop
	val (path, npi) = tovisit.dequeue
	//println("popped " + path + " " + mprint(npi))
	if (!visited(npi) && path.length > 0) println("reached " + mprint(npi) + " path = " + path)
	//if (visited(npi)) println("reached " + mprint(npi) + " path = " + path + "\nvisited = " + visited.map{mprint(_)} + "\ntovisit = " + tovisit.map{el => (el._1, mprint(el._2))})
	//else {
	  visited += npi
	  val fnpi = f(npi)
	  val gnpi = g(npi)
	  val hnpi = h(npi)
	  //println("npi = " + List(npi, fnpi, gnpi, hnpi).map{p => mprint(p)})
	  //if (!visited(fnpi) && !path.endsWith("f")) tovisit.push((path+"f", fnpi))
	  //if (!visited(gnpi) && !path.endsWith("g")) tovisit.push((path+"g", gnpi))
	  //if (!visited(hnpi) && !path.endsWith("h")) tovisit.push((path+"h", hnpi))

	  if (!visited(fnpi) && !path.endsWith("f")) tovisit.enqueue((path+"f", fnpi))
	  if (!visited(gnpi) && !path.endsWith("g")) tovisit.enqueue((path+"g", gnpi))
	  if (!visited(hnpi) && !path.endsWith("h")) tovisit.enqueue((path+"h", hnpi))

	//}
      }  
      if (visited.size > 8) 
	println("at " + mprint(pi) + " visited " + visited.size)
    }
    println( "Time (s): " + ( System.currentTimeMillis() - start ) / 1000. )
  }
}
