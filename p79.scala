object P79 {

val keys = List(319,680,180,690,129,620,762,689,762,318,368,710,720,710,629,168,160,689,716,731,736,729,316,729,729,710,769,290,719,680,318,389,162,289,162,718,729,319,790,680,890,362,319,760,316,729,380,319,728,716).map{_.toString}

val n = keys.length

val elems = keys.foldLeft("")(_+_).toSet.toList
val nelems = elems.size
val indices = List.range(0,nelems)
//order(i)(j) = 1 if elems(i) is followed by elems(j) 
//order(i)(j) = -1 if conflict (\exists key1 s.t. elems(i) appears before elems(j) and key2 s.t. elems(j) appears before elems(i) 
val order = Array.ofDim[Int](nelems, nelems)
indices foreach {
  i => 
  List.range(i+1, nelems) foreach {
    j =>       
      val matchingKeys = keys.filter(k => (k.indexOf(elems(i)) > -1 && k.indexOf(elems(j)) > -1))
      if (matchingKeys.exists(k => k.indexOf(elems(i)) > k.indexOf(elems(j))))
	if (matchingKeys.exists(k => k.indexOf(elems(i)) < k.indexOf(elems(j)))) order(i)(j) = -1 else order(i)(j) = -2
      else order(i)(j) = 1 
  }
}

val ijp = order.map{a => a.toList.zipWithIndex}.toList.zipWithIndex.map{p => p._1.map{el => if (el._1 == 1) (el._1, List(p._2, el._2)) else (el._1, List(el._2, p._2))}}.flatten.filter{x => x._2(0) != x._2(1)}.filter{x => x._1 != 0}.map{x => x._2}

  def transf(s: List[List[Int]], lim: Int) = {
    var ns = List[List[Int]]()
    val pairs = s
    var aux = s
    var i = 0
    while (i < lim) {
      aux foreach {
	e =>
	  var res = pairs.filter(l => l(0) == e.last).toList
	res foreach {
	  e2 =>
	    val ne = e ::: e2.tail
	  ns = ns :+ ne 
	}
      }
      ns
      println(i + " : " + ns)
      aux = ns
      ns = List[List[Int]]()
      i += 1
    }
    aux
  }

var nijp = ijp
var aux = List[List[Int]]()
while (nijp != aux) {
println(nijp)
aux = nijp
nijp = transf(aux)
}

def matches(k: String, s: String) = {
  val pos = k.map{ki => s.indexOf(ki)}
  pos == pos.sorted
}

val secret = transf(ijp, 6).head.map{elems(_)}.mkString

if (!keys.map{k => matches(k, secret)}.exists(_==false)) println("found secret " + secret)

}
