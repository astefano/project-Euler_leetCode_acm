object P61 {

def tri(n: Int) = n*(n + 1)/2
def sq(n: Int) = n*n
def penta(n: Int) = n*(3*n - 1)/2
def hexa(n: Int) = n*(2*n - 1)
def hepta(n: Int) = n*(5*n - 3)/2
def octa(n: Int) = n*(3*n - 2)

  val t = List.range(1,150).map(n => tri(n)).filter(v => v > 999 && v < 10000 )
  val s = List.range(1,100).map(n => sq(n)).filter(v => v > 999 && v < 10000 )
  val p = List.range(1,100).map(n => penta(n)).filter(v => v > 999 && v < 10000 )
  val he = List.range(1,100).map(n => hexa(n)).filter(v => v > 999 && v < 10000 )
  val hp = List.range(1,100).map(n => hepta(n)).filter(v => v > 999 && v < 10000 )
  val o = List.range(1,100).map(n => octa(n)).filter(v => v > 999 && v < 10000 )

def cover(l: List[Int]) = {
  var ist = 0
  var iss = 1
  var isp = 2
  var ishe = 3
  var ishp = 4
  var iso = 5
  val m = Array.ofDim[Boolean](6,l.length)
  List.range(0,l.length) foreach {
    i => 
      if (t contains l(i)) m(ist)(i) = true
      if (s contains l(i))  m(iss)(i) = true
      if (p contains l(i)) m(isp)(i) = true
      if (he contains l(i)) m(ishe)(i) = true
      if (hp contains l(i)) m(ishp)(i) = true
      if (o contains l(i)) m(iso)(i) = true
  }
  //println(m.toList.foldLeft("")(_+"\n"+_.toList))
  m
}

def needstocover(l: List[Int]) = {
val m = cover(l)
val ways = List.range(0,6).map(i => List.range(0,l.length).filter(j => m(i)(j)==true))
if (ways.exists(_==List())) List.range(0,6).filter(i => ways(i) == List()).toSet
else {
  val r = ways partition (_.length > 1)
  println("r = " + r)
  val aux = List.range(0, l.length).toSet -- r._2.flatten.toSet
  if (aux.size == r._1.length) Set() else aux
}
}

def coverx(n: Int, np: Int) = np match {
case 0 => (t contains n)
case 1 => (s contains n)
case 2 => (p contains n)
case 3 => (he contains n)
case 4 => (hp contains n)
case 5 => (o contains n)
case _ => throw new Error("unknown p")
}

def isCirc(l: List[Int]) = l.last % 100 == l.head / 100 && !List.range(0, l.length-1).exists(i => l(i) % 100 != l(i+1) / 100)

def getCirc(l: List[Int]) = l.permutations.toList.filter(isCirc(_))

val resf = for(r4 <- res.filter(x => needstocover(x).length == 2); r2 <- cands.filter(e => (coverx(e._1, x(0)) && coverx(e._2, x(1))) || (coverx(e._1, x(1)) && coverx(e._2, x(0)))); val l = (r4 :+ r2._1) :+ r2._2; val circ = getCirc(l); if (!(r4 contains r2._1) && !(r4 contains r2._2) && circ.length!=0) ) yield circ(0)



def main(args: Array[String]) {
  val tspho = List.range(1,150).map(n => Set(tri(n), sq(n), penta(n), hexa(n), hepta(n), octa(n))).toSet.flatten.filter(v => v > 999 && v < 10000 )
  val cand = List.range(10, 100)  

  var x1 = 10
  while(x1 < 100) 
//  val res = for(x1 <- cand; x2 <- cand; x3 <- cand; x4 <- cand; x5 <- cand; x6 <- cand; val n1 = x1*100+x2; val n2 = x2*100+x3; val n3 = x3*100+x4; val n4 = x4*100+x5; val n5 = x5*100 + x6; val n6 = x6*100 + x1; if (Set(n1,n2,n3,n4,n5,n6) subsetOf tspho)) yield Set(n1, n2, n3, n4, n5, n6)

  val res3 = for(x1 <- cand; x2 <- cand; x3 <- cand; x4 <- cand; val n1 = x1*100+x2; val n2 = x2*100+x3; val n3 = x3*100+x4; if (n1 != n2 && n2 != n3 && n3 != n1 && (Set(n1,n2,n3) subsetOf tspho))) yield List(n1, n2, n3)

  val res6 = for(l <- res3; val nl = needstocover(l); r <- res3.filter(ri => (needstocover(ri).asInstanceOf[Set[Int]] intersect nl.asInstanceOf[Set[Int]]) == Set()); val lr = l ++ r; if ((l.toSet intersect r.toSet) == Set() && (l.last % 100 == r.head / 100 || l.head / 100 == r.last % 100)) ) yield lr

  res6.filter(l => (l.head / 100 == l.last % 100) && (needtocover(l) == Set()) && isCirc(l))

  println(res6)
}

}
