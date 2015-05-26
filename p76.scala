object P76 {

def gen(s: String) = {
val ln = s.length
if (ln == 2) Set(s)
else {
  var res = Set( ( s(0)+s(1)-2*'0' ) + s.drop(2))
  List.range(1,ln-1) foreach {
    i => 
      val m = s(i-1) - '0'
      val nv = s(i) + s(i+1) - 2*'0'
      if (nv <= m) res = res + ( s.take(i) + nv + s.drop(i+2))
  }
  res
}
}

val n = 100

  def countWays(n: Int) = {
    var cs = Set(List.range(0,n).map{x => 1}.mkString)
    var nw = cs.size
    var i = 1
    while(i < n-1) {
      var ns = Set[String]()
      cs foreach {
	s => 
	  ns = ns ++ gen(s)
      }
      //println(ns)
      nw += ns.size
      cs = ns
      i += 1
    }
    nw
  }

}
