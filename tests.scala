
val mod = 100000007

def f(b: List[Long], i: Int, j: Int, c: Int) : Long = {
      val n = b.length
      val costsI1 = List.range(0,i) map { ni => b(ni) + c*(i-ni) }
	println("costsI1 = " + costsI1) 
      val minI1 = if(costsI1.length > 0) costsI1.min else -1
      val costsI2 = List.range(i+1,n) map { ni => b(ni) + c*(ni-i) }
	println("costsI2 = " + costsI2) 
      val minI2 = if(costsI2.length > 0) costsI2.min else -1
	println("mini1 = " + minI1 + " mini2 = " + minI2)
      val ni = if((minI1+1)==0) (costsI2.indexOf(minI2) + i + 1) else if (minI1 < minI2 || (minI2+1)==0) costsI1.indexOf(minI1) else (costsI2.indexOf(minI2) + i + 1)
      val costsJ1 = List.range(0,j) map { nj => b(nj) + c*(j-nj) }
	println("costsJ1 = " + costsJ1) 
      val costsJ2 = List.range(j+1,n) map { nj => b(nj) + c*(nj-j) }
	println("costsJ2 = " + costsJ2) 
      val minJ1 = if(costsJ1.length > 0) costsJ1.min else -1
      val minJ2 = if(costsJ2.length > 0) costsJ2.min else -1
	println("minj1 = " + minJ1 + " minj2 = " + minJ2)
      val nj = if((minJ1+1)==0) (costsJ2.indexOf(minJ2) + j + 1) else if (minJ1 < minJ2 || (minJ2+1)==0) costsJ1.indexOf(minJ1) else (costsJ2.indexOf(minJ2) + j + 1)
	println("ni = " + ni + " nj = " + nj)	
      val aux = if (ni == nj) b(j)*(b(i)) else 0
	println("aux = " + aux)	
      def abs(v: Int, v1: Int) = if (v < v1) (v1 - v) else (v - v1)
	b(i)*(b(i)-1) + c*abs(ni, i)*b(i) + b(i)*b(ni) + b(j)*(b(j)-1) + c*abs(nj, j)*b(j) + b(j)*b(nj) + aux
      }


f(List(5,5,5),0,2,10)

f(List(5,5,5,5),0,3,10)

f(List(5,50,1,50,5),0,4,10)

f(List(5,50,1,50,5),0,4,1000)

f(List(5,50,1,50,5),1,3,1000)

f(List(4,1,1,1000,1,1), 0,4,1)

f(List(157,10,157,979797,152152152,156,4,77,157,79),3,4,123)

f(List(346455317,453638062,491871419,297060164,426458223,53746370,422461742,231053793,309679268,297959075,
 76653026,277375296,411684823,427164497,257399925,224643292,114988354,457289888,51199847,199807287,
 110648220,303379857,435712111,245040291,401790144,260999362,6953083,385721020,438059362,434000869,
 481788278,70215282,135651128,68577856,448298562,11191117,408997160,407134756,10781024,267655550,
 183460325,284786399,222774818,193425138,51658225,117130718,352764522,342521474,147243649,265160879), 2,44,9986066)

/*OBS: ce fac e over-approx: se prea poate sa treb sa schimb min dupa ce mut un nr de blocuri...*/

def f2(tr: List[String]) = {
	val n = tr.length
	val rulesM = List.range(1,n+1).map(i => (i -> (tr(i-1).split(" ").map(_.toInt).toList))).toMap
	var init = List(1)
	var next = rulesM(1)
	var count = 0
	val bound = 1000000
	var found = false
	var visited = Set(1)
	while (!found && count < bound) {
		count = count + 1	
		next = 	(init map { el => rulesM(el) }).flatten
		if (next.toSet subsetOf visited) found = true 
		visited = visited ++ next.toSet
		if (count < 10) println("next =" + next + " visited = " + visited)
		init = next
	}
	if (count < bound) println("found " + next.size) else println("unbounded")
}

f2(List("1"))

f2(List("1 1"))

f2(List("2", "3", "1"))

f2(List("1", "3 4", "2", "2"))

f2(List("2 2", "3", "4 4 4", "5", "6", "7 7 7 7", "7"))

f2(List("2 3","5 7","2 4","5","6","4","7"))

def minDucks(ducks: List[Int]) = {
val m = ducks.min
val M = ducks.max
M - m - ducks.length + 1 
}

minDucks(List(5,3,2))

minDucks(List(58))

minDucks(List(9,3,6,4))

minDucks(List(7, 4, 77, 47, 74, 44))

minDucks(List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))

def maxBeauty(chains: List[String]) = {
	val res1 = chains.partition(c => c.startsWith(".") && c.endsWith("."))
	// forms .x.
	val dotVdot = res1._1
	val res2 = res1._2.partition(_.startsWith("."))
	val res3 = res2._2.partition(_.endsWith("."))
	val res4 = res3._2.partition(!_.contains("."))
	val good = res4._1
	val leftOrRight = res4._2 
	val left = res2._1
	val right = res3._1
	def sum(x: String) = x.toList.foldLeft(0)(_-48+_)
	val maxL0 = (if (left.length > 0) left.maxBy(x => sum(x.filter(xc => xc!='.'))) else "")
	val maxLV0 = sum(maxL0)
	val maxR0 = (if (right.length > 0) right.maxBy(x=>sum(x.filter(xc => xc!='.'))) else "")
	val maxRV0 = sum(maxR0)

	val left1 = (leftOrRight.filter(_.takeRight(1).toInt > maxLV0) map {_.takeRight(1)}).zipWithIndex.sorted
	val right1 = (leftOrRight.filter(_.take(1).toInt > maxRV0) map {_.take(1)}).zipWithIndex.sorted

	var maxL = maxL0 
	var maxR = maxR0 
	if (left1.length > 0 && right1.length == 0) maxL = left1.last._1
	else if (left1.length == 0 && right1.length > 0) maxR = right1.last._1
	else if (left1.length > 0 && right1.length > 0) {
		if (left1.last._2 != right1.last._2) { 
			maxL = left1.last._1
			maxR = right1.last._1
		}
		else if (left1.last._1 + maxR > right1.last._2 + maxL) {
			maxL = left1.last._1 
			if (right1.length > 1) maxR = right1.reverse.tail.last._1
		} 
		else {
			maxR = right1.last._1 
			if (left1.length > 1) maxL = left1.reverse.tail.last._1
		} 
	}

	def maxc = ( maxL + good.foldLeft("")(_+_) + maxR + dotVdot.foldLeft("")(_+_))
	println("left = " + left + "\nright = " + right + "\ngood = " + good + "\nleftOrRight = " + leftOrRight + "\nleft1 = " + left1  + "\nright1 = " + right1 + "\nmaxc = " + maxc)
	sum(maxc.split('.').maxBy(sum(_)))
}

maxBeauty(List(".15", "7..", "402", "..3"))

maxBeauty(List("..1", "7..", "567", "24.", "8..", "234"))

maxBeauty(List("...", "..."))

maxBeauty(List("16.", "9.8", ".24", "52.", "3.1", "532", "4.4", "111"))

maxBeauty(List("..1", "3..", "2..", ".7."))

maxBeauty(List("412", "..7", ".58", "7.8", "32.", "6..", "351", "3.9", "985", "...", ".46"))

maxBeauty(List("3..", "..3"))

maxBeauty(List("5.5"))

def binRepr(n: Int, len: Int) = {
  def binReprAux(n: Int, res: List[Int]) : List[Int] = {	
    if (n==0) res
    else binReprAux(n>>1, res:+(n&1))
  }

  val aux = binReprAux(n, List()) 
  val l = aux.length
  aux:::List.fill(len-l)(0)
}

def genAllBoolFct(n: Int) = {
def genAllBoolFctAux(n: Int, res: List[List[Int]]) : List[List[Int]] = {
if (n == 1) res
else genAllBoolFctAux(n-1, (res map {i => List((i :+ 0), (i :+ 1))}).flatten)
}
genAllBoolFctAux(n, List(List(0),List(1)))
}

def genAllBoolFct2(l: List) = {
def genAllBoolFctAux(l: List, res: List[List[Int]]) : List[List[Int]] = {
if (l.length == 1) res
else genAllBoolFctAux(l.tail, (res map {i => List((i :+ 0), (i :+ 1))}).flatten)
}
genAllBoolFctAux(l, List(List(0),List(1)))
}

def intB(b: Boolean) = if (b) 1 else 0

def equiv(p: Boolean, q: Boolean) = (!p || q) && (!q || p)

def numSol1(n: Int) : Int = {
	if (n==1) 2
	else if (n==2) 3
	else if (n==3) 5
	else if (n==4) 8
	else numSol1(n-2) + 2*numSol1(n-3)+ numSol1(n-4)
}

/*inneficient*/
def allValid0(n: Int, m: Int) = {

	if(n==1) numSol1(m) 
	else {
	val indices = List.range(0,m)
	val combs = List.range(0,1<<m)
	val binReprList = combs map {
		i => 
		val aux = binRepr(i)
		val l = aux.length
		val bin = aux:::List.fill(m-l)(0)
		bin
		}

	def valid(bin: List[Int], n: Int, m: Int, evenMap: Map[Int, Boolean]) : Int = {
	if (n == 1) { 
		//println("end bin = " + bin + " evenMap = " + evenMap)
		if (evenMap.filter(_._2 == false)!=Map()) 0 else 1 
	}
	else {
	val parts = indices.partition(bin(_)==0)
	val colPos = parts._1.toSet
	val evenNeighs = (colPos map { p => if (p == 0) (p -> (evenMap(p) && !colPos(1))) else if (p == (m - 1)) (p -> (evenMap(p) && !colPos(m-2))) else (p -> (evenMap(p) && equiv(colPos(p-1), colPos(p+1)))) }).toMap 

	val next = binReprList.filter{ binCand => colPos.foldLeft(true)( (r,c) => (r && (if (evenNeighs(c)) (binCand(c) == 1) else (binCand(c)==0) )) ) } 

	val newEvenNeighs = (next map { nbin => (nbin -> (indices map {i => if (nbin(i) == 1) (i -> true) else if (colPos(i)) (i -> false) else (i -> true) }).toMap) }).toMap

	//println(" bin = " + bin + " next = " + next)

	//next.foldLeft(0)( (r,c)=>( r + valid(c, n-1, m, newEvenNeighs(c)) ) )
	val vi = next map { bin => valid(bin, n-1, m, newEvenNeighs(bin)) }
	vi.foldLeft(0)(_+_)
	}
	}

	val evenMap = (indices map {i => (i -> true)}).toMap

	//indices.foldLeft(0)( (r, c) => (r + valid(binReprList(c), n, m, evenMap) ) )

	val vi = combs map { 
		i => 
		//println(" start bin = " + binReprList(i))
		valid(binReprList(i), n, m, evenMap) 
	}

	vi.foldLeft(0)(_+_)
	}
	}

def gen(l: List[Int]) : List[Int] = {
	def genAux(l: List[Int], res: List[Int]) : List[Int] = {
		if (l == List()) res
		else genAux(l.tail, (res map { r => List(r|(1<<l.head), r) }).flatten)
	}
	genAux(l, List(0))
}

import scalax.io._
import scalax.file.{FileOps, Path, NotFileException}

def allValid(n: Int, m: Int) = {
  if(n==1) numSol1(m) 
  else {
    val aux = "out.txt"
    val path = Path (aux)
    path.createFile(failIfExists=false)
    val file: FileOps = Path (aux)		   

    val indices = List.range(0,m)
    val combs = Array.range(0,1<<m)
    val info = combs map { 
      c => 
	val cols = (for(i<-indices; if ((c>>i&1) == 1)) yield i).toSet 
      val blanks = indices.toSet -- cols
      val odds = cols.filter(p => ((p==0 && cols(1)) || (p==m-1 && cols(m-2)) || (cols(p-1) && !cols(p+1)) || (cols(p+1) && !cols(p-1))))
      val succ = gen(blanks.toList)
      (cols, odds.foldLeft(0)(_|1<<_), blanks, succ)
    }

    def valid(i: Int, n: Int, m: Int, odds: Int, succ: List[Int]) : Int = {
      val iOdds = info(i)._2
      if (n == 1) { 
	//println("end i = " + binRepr(i,m) + " odds = " + binRepr(odds, m) + " iOdds = " + binRepr(iOdds,m) + " good = " + ((odds^iOdds) == 0))
	if ((odds^iOdds) == 0) 1 else 0
      }
	else {
	  val iBlanks = info(i)._3
	  val mask = iOdds|odds
	  val vi = succ map { 
	    s => 
	      val ns = mask|s		
	    valid(ns, n-1, m, ns&iOdds, info(ns)._4) 
	  }
	  vi.foldLeft(0)(_+_)
	}
    }

    val vi = combs map { 
      i => 
	//println("start i = " + binRepr(i,m) + " succ = " + (info(i)._4 map { x => binRepr(x, m) }))
	valid(i, n, m, 0, info(i)._4) 
    }

    vi.foldLeft(0)(_+_)
  }
}


def allValid(n: Int, m: Int) = {
  if(n==1) numSol1(m) 
  else {
    val indices = List.range(0,m)
    val combs = Array.range(0,1<<m)
    val info = combs map { 
      c => 
	val cols = (for(i<-indices; if ((c>>i&1) == 1)) yield i).toSet 
      val blanks = indices.toSet -- cols
      val odds = cols.filter(p => ((p==0 && cols(1)) || (p==m-1 && cols(m-2)) || (cols(p-1) && !cols(p+1)) || (cols(p+1) && !cols(p-1))))
      val succ = gen(blanks.toList)
      (cols, odds.foldLeft(0)(_|1<<_), blanks, succ)
    }

    val ncols = List.range(0,n)
    val ncols1 = List.range(0,n-1)
    var dp: Array[Array[(Int,Int)]] = new Array(n,1<<m)
    for (i <- ncols) {
      for (c <- combs) {
	dp(i)(c) = (0,0)
      }
    }
    dp(0)(0) = (1,0)      

    for (i <- ncols1) {
      for (c <- combs) {
	val odds = info(c)._2
	for (s <- info(c)._4) {
	  val mask = odds|dp(i)(c)._2
	  val ns = s|mask
	  //println("i+1 = " + (i+1) + " ns = " + ns)
	  dp(i+1)(ns) = ( (dp(i+1)(ns)._1 + dp(i)(c)._1) % mod, dp(i+1)(c)._2|odds )
      }
    }
    }

    val ans = for(c <- combs; if ( ((dp(n-1)(c)._2)^(info(c)._2)) == 0)) yield dp(n-1)(c)._1
    ans.foldLeft(0)((r,c) => ((c + r) % mod))
  }
}

/*inefficient*/
def nways(s: Int, n: Int) : Int = {
if (n == 1) 1
else if (n == 2) (s+1)
else if (n == 3) ((s+1)*(s+2)/2)
else if (n == 4) ((s+1)*(s*s + 5*s + 6)/6)
else {
//List.range(0, s+1).foldLeft(0)((r, c)=>(r + nways(c, n - 1) % mod)) % mod
List.range(0, s+1).foldLeft(0)((r, c)=>(r + nways(c, n - 1))) 
}
}

def ways(s: Int, n: Int) : List[List[Int]] = {
if (n == 1) List(List(s))
else { List.range(0, s+1).foldLeft(List[List[Int]]())((r, c )=> (r ::: (ways(c, n - 1) map {(s-c) +: _} )))
} }

def nways2(s: Int, n: Int) : Int = {
if (n == 1 || s == 0) 1
else if (n == 2) (s+1)
else if (n == 3) ((s+1)*(s+2)/2)
else if (n == 4) ((s+1)*(s*s + 5*s + 6)/6)
else ( nways2(s, n-1) + nways2(s-1, n) ) % mod
}

def allWays(n: Int) = {

var m: Array[Array[Int]] = new Array(n,n)

val lines = List.range(1,n)

m(0)(1) = 1

for (i <- lines) {
  m(0)(i) = 1
  m(i)(1) = 1
  m(i)(2) = i + 1
  m(i)(3) = (i+1)*(i+2)/2
  m(i)(4) = (i+1)*(i*i + 5*i + 6)/6
}

val sj = List.range(5,n)

for (i <- lines; j <- sj) m(i)(j) = ( m(i-1)(j) + m(i)(j-1) ) % mod

m
}


def nRoads(n: Int, m: Int, k: Int) = {

val r = allWays(scala.math.max(n,m))

val indices = List.range(0,n)

var nr: Array[Array[Array[Int]]] = new Array(n,m,m)

val lines = List.range(1,n)
val vals = List.range(0,m)
val evenVals = List.range(0,m).filter(_ % 2 == 0)

for (j <- lines; i <- evenVals) nr(j)(i)(0) = 1

for (j <- lines; i <- evenVals; p <- List.range(i,m+1)) {
  val pos = indices.filter(scala.math.abs(_,i) <= k).length
  nr(j)(i)(p) = evenVals.foldLeft(0)( (res,c) => ( res + ( r(c)(pos) * nr(j-1)(c)(p-i) ) ) ) % mod
}


def ce(w: List[Int], s: Int) : Int = {
  val n = w.length
  if (n == 3) s + w(0)*w(2)
  else {
    val indices = List.range(1, n-1)
    val l = 
	for(i <- indices; val ns = s + w(i-1)*w(i+1)) yield ce(w.take(i):::w.drop(i+1), ns)
    l.foldLeft(0)((r,c) => (if (c >= r) c else r))
    }
}

ce(List(1,2,3,4), 0)

ce(List(100,2,1,3,100), 0)

ce(List(2,2,7,6,90,5,9), 0)

ce(List(477,744,474,777,447,747,777,474), 0)

ce(List(1,1,1,1,1,1,1,1,1,1),0)

ce(Array.fill(20)(1000).toList, 0)

def maxL(l: List[Int]) = l.foldLeft(0)((r,c) => (if (c > r) c else r))

def minL(l: List[Int]) = l.foldLeft(0)((r,c) => (if (c < r) c else r))

def delete(l: List[Int], i: Int) = l.take(i):::l.drop(i+1) //; println("del: l = " + l + " i = "  + i + " r = " + r); r}


def ceI(w: List[Int]) = {
val n = w.length
var l : Array[Map[List[Int],Int]] = new Array(n)

val range = List.range(1,n-1)

val v = w(0)*w(n-1)
l(0) = (range map (i => (List(i) -> v))).toMap

//println("l0 = " + l(0))

val l0h = l(0).keys map {_.head}
l(1) = (for (i <- l0h; j <- l0h.filter(_>i)) yield (List(i,j) -> (v + scala.math.max(w(0)*w(j), w(i)*w(n-1))))).toMap

//println("l1 = " + l(1))

for (i <- List.range(2, n-3)) {
  l(i) = Map()
  val ni = l(i-1).toList.length
  val prevL = l(i-1).keys.toList
 // println("i = " + i  + " ni = " + ni + " prevL = " + prevL + " i1 range = " + List.range(0, ni))
  for (i1 <- List.range(0, ni); i2 <- List.range(0, ni)) {
    val suff1 = prevL(i1).takeRight(i-1)
    val pref2 = prevL(i2).take(i-1)
    if (suff1 == pref2) {
      val suff2 = prevL(i2).drop(i-1)
      val nel = prevL(i1):::suff2
      //println("i1 = " + i1 + " i2 = " + i2 + " nel = " + nel)
      val nvals = List.range(1,i) map { 
	p => 
	  val neli = delete(nel, p)
	  //println("neli = " + neli + " l(i-1).keys(neli) = " +  l(i-1).keys)
	  w(nel(p-1))*w(nel(p+1)) + l(i-1)(neli) }
      //println("nvals = " + nvals)
      val nmaxf = w(0)*w(nel(1)) + l(i-1)(nel.tail)
      val nmaxl = w(n-1)*w(nel(i-1)) + l(i-1)(nel.dropRight(1))
      //println("nmaxf = " + nmaxf + " nmaxl = " + nmaxl)
      l(i) = l(i).updated(nel, scala.math.max(nmaxf, scala.math.max(nmaxl, maxL(nvals))))
      //println("l" + i + " = " + l(i))
    }
}//end inner for
}//end outside for
val r = (l(n-4) map {_._2}).toList
val keys = (l(n-4) map {_._1}).toList
val indS = List.range(1,n-1).toSet
val nr =  keys map {
  k => 
  val missingS = (indS -- k.toSet)
  //println("k = " + k + " missing = " + missingS)
  val missing = missingS.toList(0)
  l(n-4)(k) + w(missing - 1) * w(missing + 1)
}
maxL(nr)
}//end ceI

ceI(List(2,2,7,6,90,5,9))

ceI(List(477,744,474,777,447,747,777,474))

ceI(List(1,1,1,1,1,1,1,1,1,1))

ceI(Array.fill(20)(1000).toList)

var count = 0 

def arrange(files: List[String]) : List[String] = {
val n = files.length
if (files.last == "." && files(n-2) == "..") files
else {
  val i1 = files.indexOf(".")
  val j1 = files.indexOf("..")
  val res = if (i1 < j1 || (j1 + 1) == -1) ((files.take(i1):+files.last):::(files.drop(i1+1).dropRight(1))):+"."
	    else ((files.take(j1):+files.last):::(files.drop(j1+1).dropRight(1))):+".."

  if (res.last == "." && res(n-2) == "..") res
  else {
    val i2 = res.indexOf(".")
    val j2 = res.indexOf("..")
    val res2 = if (i2 < j2 || (j2 + 1) == -1) ((res.take(i2):+res(n-2)):::(res.drop(i2+1).dropRight(2))):+res.last:+"."
	       else ((res.take(j2):+res(n-2)):::(res.drop(j2+1).dropRight(2))):+res.last:+".."
    res2
  }
}
}

arrange(List("ContestApplet.jnlp", ".", "Image.jpg", "..", "Book.pdf", "Movie.avi"))

arrange(List("Image.jpg", "..", "."))

arrange(List("..", ".", "Image.jpg"))

arrange(List("No", "..", "Zaphod", ".", "Just", "very", "very...", "Improbable"))

arrange(List("www.topcoder.com", "Ever.tried", ".", "Ever.failed", "..", "No", "Matter.", "Try", "Again.", "Fail", "Again..", "Fail.Better"))

arrange(List("This", ".", "is", "tricky", "test", ".."))

/*BIT OPS: get the pos of the least significant bit as in:
http://stackoverflow.com/questions/757059/position-of-least-significant-bit-that-is-set
*/
val MultiplyDeBruijnBitPosition = List(0, 1, 28, 2, 29, 14, 24, 3, 30, 22, 20, 15, 25, 17, 4, 8, 31, 27, 13, 23, 21, 19, 16, 7, 26, 12, 18, 6, 11, 5, 10, 9)

def getFirstBit(x: Int) = MultiplyDeBruijnBitPosition(((v & -v) * 0x077CB531) >> 27)

/* BIT OPS
 * 23 = 10111 with 1st one being on pos 4, 0 on pos 3, 2nd 1 on pos 2, 3rd 1 on pos 1, 4th 1 on pos 0
  *to get bit i we do & with 2^i, i.e., with (1<<i). if n&(1<<i) = 0 => the ith bit is 0, owise, is 1. 
  * to get the last bit we do & with 1, i.e., 00000..01.
  * n>>1<<1 changes the last bit from 1 to 0. (so the new val is n-1)
  * n<<1|1 shifts n with 1 pos to the left and adds 1, i.e., if n = b_1...b_k, n<<1|1 = b_1...b_k1
  * */

def countGameSteps(i: Int, steps: Int) : Int = {
if (i == 0) steps 
else {
  //println("i = " + i)
  if ((i&1) == 1) countGameSteps(i>>1<<1, steps)
  else {
    var aux = i
    var zeros = 0
    //find the last bit 1
    while ((aux&1) == 0) {
      zeros = zeros + 1
      aux = aux>>1
    }
    //see if the last 2 bits after the last one are 1 (then a jump of 2 is poss)
    if ((aux&(1<<1))!=0 && (aux&(1<<2))!=0) {
      /*i is of the form aux000..0 with k ending zeros, and aux = x111
       * we need to transf i = x11100..0 into x01110..0 with (k-1) ending zeros
       * we do this by aux = aux>>3, aux = aux<<2 (aux becomes x00) aux = x|1 (aux becomes x01)
       * aux = aux<<1|1 (aux becomes x011), aux = aux<<1|1 (aux becomes x0111)
       * this is the same as aux>>3<<4|7 (i.e., x0000|00111)
       * now we get the new i, aux<<(k-1)
       */ 
      val ni = ((aux>>3<<4)|7)<<(zeros-1)
      countGameSteps(ni, steps+1)
    }
    //else aux = x01 so we change i = x010..0 into x0010..0 with k-1 ending zeros
    else countGameSteps((((aux>>1<<2)|1)<<(zeros-1)), steps+1)
  }
}
}

def transfBoardInt(board: String) = {
var r = 0 
board foreach {
  bi => 
    if (bi == '.') r = r<<1 else r = r<<1|1
}
r
}

//if the number of steps is odd then the 1st player wins, else the second
def getWinner(board: String) = if ((countGameSteps(transfBoardInt(board), 0) % 2) == 1) "yes" else "no"

transfBoardInt(".o...")

getWinner(".o...")

getWinner("..o..o")

getWinner(".o...ooo..oo..")

getWinner("......o.ooo.o......")

getWinner(".o..o...o....o.....o")

getWinner(".o..o...o....o..........o...o..oo.......o")

def getABC(x: Int, y: Int, z: Int, t: Int) = {
val xyzt = x + y + z + t
if ((xyzt % 2) == 1) List[Int]()
else {
val aPb = if (x == xyzt/2) x else if (y == xyzt/2) y else if (z == xyzt/2) z else t
val unknown = if (aPb == x) List(y,z,t) else if (aPb == y) List(x,z,t) else if (aPb == z) List(x, y, t) else List(x, y, z)
val u1 = unknown(0) + unknown(1)
val u2 = unknown(0) + unknown(2)
val u3 = unknown(1) + unknown(2)
var a = -31
var b = -31
var c = -31
// i assume u1 is bMc + bPc
if (u1 % 2 == 0) { b = u1/2; if (unknown(0)-b >= 30) c = unknown(0)-b else c = unknown(1)-b }
else if (u2 % 2 == 0) { b = u2/2; if (unknown(0)-b >= 30) c = unknown(0)-b else c = unknown(2)-b }
else if (u3 % 2 == 0) { b = u3/2; if (unknown(1)-b >= 30) c = unknown(1)-b else c = unknown(2)-b }
a = aPb - b
List(a,b,c)
}
}

getABC(1,-2,3,4)

getABC(0,0,5,5)

getABC(10, -23, -10, 3)

getABC(-27, 14, 13, 22)

getABC(30, 30, 30, -30)

/*scala.Long.MaxValue is the Long 9223372036854775807
 * scala.Long.MaxValue < 10^18 - 1
 * scala.Long.MaxValue < 2^63
 */

/*Computing primes efficiently
 * from http://datacute.wordpress.com/2008/04/16/basic-primes-in-scala/
 */
def merge(xs: Stream[Int] , ys: Stream[Int]): Stream[Int] = {
  if (xs.head < ys.head)
    return Stream.cons(xs.head, merge(xs.tail, ys))
  if (xs.head == ys.head)
    return Stream.cons(xs.head, merge(xs.tail, ys.tail))
  return Stream.cons(ys.head, merge(xs, ys.tail))
}

def diff(xs: Stream[Int], ys: Stream[Int]): Stream[Int] = {
  if (xs.head < ys.head)
    return Stream.cons(xs.head, diff(xs.tail, ys))
  if (xs.head == ys.head) return diff(xs.tail, ys.tail)
  return diff(xs, ys.tail)
}

def f(xs: Stream[Int], ys: => Stream[Int]) = Stream.cons(xs.head, merge(xs.tail, ys))

def g(p: Int) = Stream.from(p*p,p*2)

def foldr1(f: (Stream[Int], => Stream[Int]) => Stream[Int]) (s: Stream[Stream[Int]]): Stream[Int] = if (s.isEmpty) Stream.empty else f(s.head, foldr1(f)(s.tail))

val primes: Stream[Int] =  Stream.fromIterator((2::3::5::Nil).elements).append(diff(Stream.from(7,2), foldr1(f)(primes.tail map g)))

lazy val primes2: Stream[BigInt] = Stream.cons(BigInt(2), primes2.map(b => new BigInt(b.bigInteger.nextProbablePrime)))

val primes3: Stream[Int] = {
    var last = 2
    2 #:: Stream.from(last+1).filter(candidate => {
      last=candidate
      primes3.takeWhile(_ < scala.math.sqrt(last).toInt+1).forall(candidate % _ != 0)
    })
  }

def test3 = {
val start = System.currentTimeMillis
println("primes(1000000) = " + primes3(1000000))
val end = System.currentTimeMillis
println("Took " + (end -start)/1000.0 + " s.")
}

def testTimePrimes = {

val start1 = System.currentTimeMillis

println("primes(100000) = " + primes(100000))

val end1 = System.currentTimeMillis

println("Took " + (end1 -start1)/1000.0 + " s.")

val start2 = System.currentTimeMillis

println("primes2(100000) = " + primes2(100000))

val end2 = System.currentTimeMillis

println("Took " + (end2 -start2)/1000.0 + " s.")

}

// Prime Factor Tuple: Prime, Power, Index
type Factors = List[(Int, Int, Int)];
def primeFactorsRecurse(i: Int, factors: Factors,
    morePrimes: Stream[Int], index: Int): Factors = {
  if (i == 1) return factors;
  if (i % morePrimes.head == 0) {
    if ((factors != Nil) && (morePrimes.head == factors.head._1)) {
      return primeFactorsRecurse(i / morePrimes.head,
          (morePrimes.head, factors.head._2 + 1, index) ::
          factors.tail,
          morePrimes, index);
    } else {
      return primeFactorsRecurse(i / morePrimes.head,
          (morePrimes.head, 1, index) :: factors,
          morePrimes, index);
    }
  }
  return primeFactorsRecurse(
      i, factors, morePrimes drop 1, index + 1);
}

def primeFactors(i: Int): Factors = {
  if (i == 1) return (1,1,0) :: Nil;
  primeFactorsRecurse(i, Nil, primes, 1);
}

def arePrimes(a: Long, b: Long) = {
val min = scala.math.min(a,b)
var i = 0
while (primes(i) < min && ((a % primes(i)) == 0 && (b % primes(i)) == 0)) i = i + 1 
if (primes(i) >= min && !((a % primes(i)) == 0 && (b % primes(i)) == 0) ) true else false
}

def findAB(g: Long, l: Long) = {
if (g > l) -1
else {
val lim = scala.math.sqrt(l) 
val lg = l/g
val vals = primes.takeWhile(_<lim).toList
//println("vals = " + vals)
val res = vals.dropWhile(p => ((l % (p * g)) != 0 || ( ((lg/p) % p) ==0)))
if (res == List()) -1
else {
println("res " + (res take 4))
val aF = res.head
val b = (l/aF)
println("a = " + (aF*g) + " b = " + b)
aF*g + b
}
}
}

findAB(2,20)
  
findAB(5,8)

findAB(1000, 100)

findAB(100, 1000)

findAB(10, 950863963000L)


def sum(l: Long) = {val newl = l.toString.toList; val n = newl.length; newl.foldLeft(0)(_+_) - 48*n }

/* how to compute the nb of numbers of p digits with the sum of the digits s: 
  * for s = 3 we have C(p,3) + C(p,2) + p
  * * the ways to combine 3 digits of 1, i.e., C(p, 3) +
  * * the ways to combine 1 digit of 1 with 1 digit of 2 (and the rest 0), i.e. C(p, 2) +
  * * the ways to combine 1 digit of 3 (and the rest 0), i.e., C(p, 1) = p.
  * for s = 4 we have C(p,4) + C(p,3) + C(p,2) + p: 
  * * the ways to combine 4 digits of 1 on p pos, i.e., C(p,4) + 
  * * the ways to combine 2 digits of 1, 1 digit of 2, on p pos, i.e. C(p,3) +
  * * the ways to combine 2 digits of 2, i.e., C(p,2) +
  * * the ways to combine 1 digit of 4, i.e., p.
  * for s = 5 we have C(p,5) + C(p,4) + 2*C(p,3) + 2*C(p,2) + p:
  * * 11111
  * * 1112
  * * 113, 221
  * * 23, 14
  * * 5
  * for s = 6 we have C(p,6) + C(p,5) + C(p,4) + 2C(p,3) + 2C(p,2) + p
  * * 111111
  * * 11112
  * * 1113
  * * 114, 222
  * * 15, 24, 33
  * * 6
  * for s = 7 we have C(p,6), C(p,5) + 2C(p,4) + 3C(p,3) + 3C(p,2) + p
  * * 1111111
  * * 111112
  * * 11113, 11122
  * * 1114, 1222, 1123
  * * 115, 124, 223
  * * 16, 25, 34
  * */

/* given s, i want to compute the number of solutions:
 * x1*1 + x2*2 + ... + x9*9 = s with x1+...+x9 < p (so p - (x1+..+x9) are the 0-digits)
 * so for each such solution we get C(p, (x1+...+x9)) numbers with p digits the sum of the digits being s.
 * */

/*returns all sols as lists of length 9. for each list li, \sum_{j=1,9} li(j)*j = s
 * it runs out of mem when, for example, s > 50 and n = 18
 */
def genSols(s: Int,n: Int) = {
var m: Array[Array[List[(List[Int],Int)]]] = new Array(10,n+1)
val indices = List.range(0, n+1)
val digits = List.range(2,10)
indices.filter{_<=s} foreach {i => m(1)(i)=List((List(i), i))}
for(d<-digits; i<-indices) {
  m(d)(i) = List()
  indices.filter(j => m(d-1)(j)!=null) foreach {
    j => 
    val r = m(d-1)(j).filter{_._2+d*i<=s}
    m(d)(i) = m(d)(i) ::: (r map {ri => (ri._1:+i, ri._2+d*i)})
  }
}
val ml = (m(9).toList.filter(_!=null).flatten.filter(_._2 == s) map {li => li._1.foldLeft(0)(_+_)}).filter(_<=n)
println("sols = " + (m(9).toList.filter(_!=null).flatten.filter(_._2 == s) map {_._1}).filter(li=> li.foldLeft(0)(_+_)<=n))
ml
}
  
def comb(n: Int, k: Int) : Long = {
if (k == 1) n 
else if (k == n) 1 
else if (n == 1) 1
else {
comb(n-1, k-1) + comb(n-1, k)
}
}

/*this runs out of mem for s = 60, n = 18*/
def computeNbSols(s: Int,n: Int) = {
var m: Array[Array[List[(Int,Int)]]] = new Array(10,n+1)
val indices = List.range(0, n+1)
val digits = List.range(2,10)
indices.filter{_<=s} foreach {i => m(1)(i)=List((i, i))}
for(d<-digits; i<-indices) {
  m(d)(i) = List()
  indices.filter(j => m(d-1)(j)!=null) foreach {
    j => 
    val r = m(d-1)(j).filter{_._2+d*i<=s}
    m(d)(i) = m(d)(i) ::: (r map {ri => (ri._1+i, ri._2+d*i)})
  }
}
//println("m9 = " + m(9).toList.filter(_!=null).flatten.filter(l => l._2 == s))
val ml = m(9).toList.filter(_!=null).flatten.filter(l => l._2 == s && l._1<=n) map {_._1}
//println("ml = " + ml)
var nd: Array[Int] = new Array(n+1) 
ml foreach { nli => nd(nli) = nd(nli) + 1 }
//println("nd = " + nd.toList)
List.range(0,n+1).foldLeft(0L)((r,c) => (nd(c)*comb(n,c) + r))
}

val solsSumS = (List.range(1, 41) map {i => (i -> computeNbSols(i, 18))}).toMap
var intervalsMap = Map[Int, (Long, Long)]()
intervalsMap = intervalsMap.updated(1, (1, solsSumS(1)))
List.range(2,41) foreach {
i => 
intervalsMap = intervalsMap.updated(i, (intervalsMap(i-1)._2+1, intervalsMap(i-1)._2 + solsSumS(i)))
}

def getEl(idx: Long) = {
val res = intervalsMap.find(x => x._2._1 <= idx && x._2._2 >= idx)
val sumIdx = res match {
  case Some(x) => x._1
  case _ => -1
}
if (sumIdx != -1) {
  val digits = List.range(0,10)
  var findD: Array[Int] = new Array(18)
  val nbP = 18
  println("sum is " + sumIdx + " init interval = " + intervalsMap(sumIdx))
  var i = 1
  List.range(0, 18) foreach {
  p => {  
    var c = intervalsMap(sumIdx)._1 
    val pre = if (p == 0) 0 else findD.take(p-1).foldLeft(0)(_+_)  
    val res = digits.filter(_<=sumIdx).takeWhile{
      d => 
	val ns = sumIdx-pre-d
	val np = nbP-1-p
	c = c + computeNbSols(ns, np)
	println("at ns = " + ns + " np = " + np + " c = " + c)
        idx > c
    }//end dropWhile
    findD(p) = if (res!=List()) res.last else 0
    println("res = " + res + " idx repr a nb starting with " + ( findD.take(p+1).toList) )
    }
  }
}
}

getEl(13)

def maxI(l: List[Int]) = l.foldLeft(0)((r,c) => (if (c > r) c else r))
def maxC(l: List[Char]) = l.foldLeft(0)((r,c) => (if (c > r) c else r))

def minDices(throws: List[String]) = {
 
  def dices(throws: List[String], res: List[Int]) : List[Int] = {
    //println("thr = " + throws)
    if (throws(0)=="") res
    else {
      val n = throws.length
      val indices = List.range(0,n)
      val maxl = throws map {_.max}
      val nthrows = indices map {
	i => 
	  val p = throws(i).indexOf(maxl(i))
	  throws(i).take(p)+throws(i).drop(p+1)
      }
      dices(nthrows, res:+(maxC(maxl)-48))
    }
  }

 val res = dices(throws, List())
 println("res = "  + res) 
 res.foldLeft(0)(_+_)
}

minDices(List("24412", "56316", "66666", "45625"))

minDices(List("931", "821", "156", "512", "129", "358", "555"))

minDices(List("3", "7", "4", "2", "4"))

minDices(List("281868247265686571829977999522", "611464285871136563343229916655", "716739845311113736768779647392", "779122814312329463718383927626",
"571573431548647653632439431183", "547362375338962625957869719518", "539263489892486347713288936885", "417131347396232733384379841536"))

minDices(List("1112", "1111", "1211", "1111"))


def combAsInts(n: Int, k: Int) : List[Int] = {
if (k == 1) List.range(0L,n) map {scala.math.pow(2,_).toInt} 
else if (k == n) List(scala.math.pow(2,n).toInt - 1) 
else if (n == 1) List()
else {
val l = combAsInts(n-1, k-1) map {_<<1|1}
val r = combAsInts(n-1, k) map {_<<1}
//println("l = " + l + " r = " + r)
l ::: r
}
}

def initCombs(n: Int, k: Int) = {
val nl = List.range(1,n)
val kl = List.range(k,n)
var combs: Array[Array[List[Int]]] = new Array(n,k)
for (nli <- nl; kli <- kl) combs(nli)(kli) = combAsInts(nli, kli)
}

def minL(l: List[Double]) = l.tail.foldLeft(l.head)((r,c) => (if (c < r && c >= 0) c else r))

def maxD(l: List[Double]) = l.foldLeft(-2000000.)((r,c) => (if (c > r) c else r))

def rewRev(rev: List[Double], k: Int) = {
val n = rev.length
val nl = List.range(1,n+1)
val kl = List.range(k,n+1)
var combs: Array[Array[List[Int]]] = new Array(n+1,n+1)
for (nli <- nl; kli <- kl) combs(nli)(kli) = combAsInts(nli, kli)

val combsL = (combs.toList map {_.toList}).flatten.filter(_!=null).flatten
println("kl = " + kl + " combsL = " + combsL)
var revN: Array[Array[List[Double]]] = new Array(n+1, maxL(combsL)+1)
for (kli <- kl; comb <- combsL) revN(kli)(comb) = rev
var rn = 0 
//for (kli <- kl; val nd = n-List.range(k,kli).foldLeft(0)(_+_); if (nd >= 0))
/*nd = n-kli+2, nd represents the new length of revenues:
 * we deleted kli and we added 1! (the other 1 is because we start counting from 0
 */ 
for(kli <- kl; val nd = (if (kli==k) n else (n-kli+2)); if (nd >= 0)) {
  println("nd = " + nd + " kli = " + kli)
  for (comb <- combs(nd)(kli)) { //; if (revN(kli)(comb).length >= kli)) {
    println(" combs = " + combs(nd)(kli) + " comb = " + comb)
    println("before: revN(" + kli + ")(" + comb + ") = " + revN(kli)(comb))
    val bits = List.range(0,revN(kli)(comb).length).partition(i => (comb&(1<<i))==0) 
    val rn = bits._2.foldLeft(0.)((r,c) => (r + revN(kli)(comb)(c)))/bits._2.length
    println("bits = " + bits + " rn = " + rn)
    revN(kli)(comb) = ( bits._1 map {i => revN(kli)(comb)(i)} ) :+ rn
    println("after: revN(" + kli + ")(" + comb + ") = " + revN(kli)(comb))
  }
}
println("revN = " + (revN.toList map{_.toList}).flatten.filter(_!=null).flatten)
maxD((revN.toList map{_.toList}).flatten.filter(_!=null) map {li => li.foldLeft(0.)(_+_)/li.length})
}

rewRev(List(5,-7,3), 2)

rewRev(List(5,-7,3), 3)

rewRev(List(1, 2, 2, 3, -10, 7), 3)

def rewRev(rev: List[Double], k: Int) = {
val n = rev.length
val nl = List.range(1,n+1)
val kl = List.range(k,n+1)
var combs: Array[Array[List[Int]]] = new Array(n+1,n+1)
for (nli <- nl; kli <- kl) combs(nli)(kli) = combAsInts(nli, kli)

val combsL = (combs.toList map {_.toList}).flatten.filter(_!=null).flatten
//println("kl = " + kl + " combsL = " + combsL)
var revN = Map[(Int,Int),List[Double]]()

def computeNewRev(rev: List[Double], comb: Int) = {
  val bits = List.range(0,rev.length).partition(i => (comb&(1<<i))==0) 
  val rn = bits._2.foldLeft(0.)((r,c) => (r + rev(c)))/bits._2.length
  ( bits._1 map {i => rev(i)} ) :+ rn
}

for (kli <- kl; comb <- combsL) revN = revN.updated((kli,comb), computeNewRev(rev, comb))
for (kli <- kl; keys <- revN.keys; val currentRev = revN(keys); if (currentRev.length >= kli)) {
val currentCombs = combs(currentRev.length)(kli)
  currentCombs foreach { 
    c => revN = revN.updated((kli, c), computeNewRev(currentRev, c))
  }
}

//println("revN = " + (revN.keys map {key => (key, revN(key), revN(key).foldLeft(0.)(_+_)/revN(key).length)}))

val res = revN.keys map {
  key => 
  val len = revN(key).length
  if (len >= k || len == 1) revN(key).foldLeft(0.)(_+_)/len
  else -200000
}

//println("res = " + res)

maxD(res.toList)
//maxD((revN.toList map{_.toList}).flatten.filter(_!=null) map {li => li.foldLeft(0.)(_+_)/li.length})
}

rewRev(List(5,-7,3), 2)

rewRev(List(5,-7,3), 3)

rewRev(List(1, 2, 2, 3, -10, 7), 3)

rewRev(List(869, 857, -938, -290, 79, -901, 32, -907, 256, -167, 510, -965, -826, 808, 890, -233, -881, 255, -709, 506, 334, -184, 726, -406, 204, -912, 325, -445, 440, -368), 7)


def computeRootsBinPoly(a: List[Int]) = {
val s1 = a.foldLeft(0)((r,c)=>(r + c))
val sol1 = if ((s1 % 2)== 0) 1 else 0
if (a.head == 0) sol1 + 1 else sol1
}

computeRootsBinPoly(List(1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 0, 1, 1))

computeRootsBinPoly(List(1, 0, 1, 1, 1, 1, 0, 0, 1, 1, 0, 1, 0, 1, 1, 0, 0, 0, 0, 0, 1, 0, 1, 1, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 1, 1, 1, 0, 1, 0, 1, 1, 1, 1, 0, 1, 0, 0, 1, 1))

val ini = valN(List(9,3,3,1,1,1,1,1,1,1,1))
val ini2 = valN(List(9,3,2,1,1,1,1,1,1,1,1))

val indices = List.range(0,primes.length)

def divs(is: List[Int]) = is.foldLeft(BigInt(1))((r,c)=>(r*(2*c+1)))

val primes = List(2,3,5,7,11,13,17,19,23,29,31,37)

//def valN(is: List[Int]) = indices.foldLeft(BigInt(1))((r,c)=>(r*(math.pow(primes(c),is(c)).toLong)))
def valN(is: List[Int]) = List.range(0,is.length).foldLeft(BigInt(1))((r,c)=>(r*(math.pow(primes(c),is(c)).toLong)))

  def f(l2:Int, L2:Int, l3:Int, L3:Int, L5:Int, L7:Int, L11:Int, L13:Int, L17:Int, L19:Int) = {
    val s2 = List.range(l2,L2+1)
    val s3 = List.range(l3,L3+1)
    val s5 = List.range(1,L5+1)
    val s7 = List.range(1,L7+1)
    val s11 = List.range(0,L11+1)
    val s13 = List.range(0,L13+1)
    val s17 = List.range(0,L17+1)
    val s19 = List.range(0,L19+1)
    val sr = List(0,1)
    val sr0 = List(0)
    val sr1 = List(1)
    val s23 = sr1
    val s29 = sr1
    val s31 = sr0
    val s37 = sr0
    val mdivs = 4*1000000L
    val sols = for (i2 <- s2; i3 <- s3.filter(_<=i2); i5 <- s5.filter(_<=i3); i7 <- s7.filter(_<=i5); i11 <- s11.filter(_<=i7); i13 <- s13.filter(_<=i11); i17 <- s17.filter(_<=i13); i19 <- s19.filter(_<=i17);  i23 <- s23.filter(_<=i19); i29 <- s29)//; i31 <- s31.filter(_<=i29); i37 <- s37.filter(_<=i31))
		  //  if divs(List(i2,i3,i5,i7,i11,i13,i17,i19,i23,i29,i31,i37)) > mdivs) 
	       //yield (valN(List(i2,i3,i5,i7,i11,i13,i17,i19,i23,i29,i31,i37)),List(i2,i3,i5,i7,i11,i13,i17,i19,i23,i29,i31,i37))
	       yield List(i2,i3,i5,i7,i11,i13,i17,i19)//,i23,i29,i31,i37)
		 println(" first sols " + sols.take(10))
		 //val m = sols.map(_._1).min
    //println("sol = " + sols.filter(_._1 == m))
    //sols
  }
 
f(2,49,1,19,10,6,4,3,1,0)

P120

 def rmax(a: Int) = {
    val a2 = a*a
    val n = List.range(1,a2/2+1)
    n.foldLeft(0)((r,c) => (if (r < (2*c*a % a2)) (2*c*a % a2) else r))
  }
 
def maxMod() = List.range(3,1001).foldLeft(0)((r,a)=>(r + rmax(a)))

res = 333082500

P123

def rem(i: Int, n: Int) = (1L*n % (primes(i)*primes(i))) * (2*primes(i)) 

def check(n: Int) = (math.pow(primes(n)-1,n+1).toLong + math.pow(primes(n)+1,n+1).toLong) % (primes(n)*primes(n))

def rem2(n: Int) = if ((n+1) % 2 == 0) 2 else (2L*(n+1) % (primes(n))) * primes(n)

def test = List.range(5,10).filter(n => rem2(n) != check(n))

def findP123(ini: Int) = {
  var n = ini
  while(rem2(n) < 10000000000L) n = n+2
  n
}

findP123(7036)

//primes(25) = 101
//below 100 there are 4 primes with the prop: p + n = k^3
//so we look at primes after pos 25
15485863

//(7,1), (19,8), (37,27), (61,64)
val cubes = List.range(1,150) map {i => i*i*i}

def solP131(ini: Int) = {
var i = ini
var sols = 4
while(primes(i) < 80000) { 
  val nsols = for(n <- cubes; val k = math.cbrt(1L*(n+primes(i))); if (k - k.toLong == 0)) yield (primes(i),n)
  if (nsols != List()) {
    println("nsols = " + nsols)
    sols = sols + nsols.length
  }
  i = i + 1
}  
sols
}

solP131(4203)

def isPrime(p: Int) = {
  val pos = primes.indexWhere(_>=p)
  primes.take(pos+1).find(_==p) match {
    case Some(x) => true
    case None => false
  }
} 

  def sol2P131 = {
    var sols = 0
    List.range(1,577) foreach {
      i => 
      val candidate = 3*i*(i+1)+1
      if (isPrime(candidate)) sols = sols + 1 
    }
    sols
  }

val cubes = List.range(1,100) map {i => i*i*i}

//prost
def sol3P131 = {
  val sols = for(r3 <- cubes; q3 <- cubes.filter(x => x>r3 && arePrimes(x,r3)); val p = q3 - r3; if isPrime(p)) yield (p)
  sols.length
}

primes.indexWhere(_>20000) => 2262

primes.indexWhere(_>30000) => 3245

primes.indexWhere(_>40000) => 4203



  def checkP131(p: Int, n: Int) = math.cbrt(1L*n*n*(n+p))

P135
def zd(z: Int, d: Int) = 2*z*d+3*d*d-z*z

def getDZ(n1: Int, n2: Int) = { val d = (n1+n2)/4;  (d, n2 - d) }

def sols(n: Int, max: Int) = {
val nsq = math.sqrt(3*n).toInt 
var s = 0
val divs = List.range(1,nsq)//.dropWhile{
var i = 0 
while(s <= 10 && i < divs.length) {
  val n2 = divs(i) 
  if ((n % n2 == 0) && ((n2 + (n / n2)) % 4 == 0)) {
    //println("n2 = " + n2 +  " (d,z) = " + getDZ(n2,n/n2))
    s = s + 1 
    }
  i = i + 1
}
if(s > max+1) println("!!!! " + s)
s
}

def solP135 = {
var ns = 0
List.range(1,1000000) foreach {
  n => 
  if (sols(n,10) == 10) ns = ns + 1
}
ns
}
// n = d (4d - y) <=> 4 | n/d + d 

def isSq(n: Int) = {
  val  nsq = math.sqrt(n)
  nsq.toInt - nsq == 0
}

def sols2(n: Int) = {
//val nsq = math.sqrt(n/3).toInt 
val l = (n+1)/4
val pos = primes.indexWhere(_>=l)
val candidates = primes.take(pos+1)
candidates.filter(d => isSq(4*d*d-n)).length
}

def sol2P135 = {
var ns = 0
List.range(1,100000) foreach {
  n => 
  if (sols2(n) == 2) ns = ns + 1
}
ns
}

def solP136 = {
//val ns = List.range(1,101)
val ns = List.range(1,101).filter(_ % 4 == 0)
println("ns = " + ns)
var unis = 0
//var uniques = List[Int]() 
ns foreach {
n => {
  val cand = List.range(1,math.sqrt(n).toInt + 1)
  var sols = 0
  cand.dropWhile{
    y => 
      if (n % y == 0 && ((n/y) + y) % 4 ==0) sols = sols + 1
    (sols < 2) 
  }
  //println("n = " + n + " sols = " + sols)
  if (sols == 1) //uniques = uniques :+ n
    unis = unis + 1
}
}
//println("uni = " + uniques)
//uniques.length
val pos = primes.indexWhere(_>100)
val unip = primes.take(pos+1).filter(_+1 % 4 == 0)
println("unip = " + unip)
unis + unip.length
}

def countPrimes4n3(bound: Int) = {
val all = List.range(1, bound+1)
var cp = 0
all foreach {
p => 
  if ((primes(p) + 1) % 4 == 0) cp = cp + 1
}
cp
}

def sol142 = {
val a = List(65, 85, 145, 185,  
val d = List(56, 77, 143, 153,
val e = List(33, 36, 24,  104,
val c = List(63, 84, 144, 176,
val f = List(16, 13, 17,  57,
}

def isPS(i: Long) = {
val r = math.sqrt(i).toLong
r*r-i == 0
}

val xy = (List.range(0,2000).filter(_%2==0) map {i => (l3(i) -> l3(i+1))}).sorted

val x = xy map {_._1}

val xyr = List.range(1,999).filter(i => (xy(i)._1 == xy(i+1)._1 || xy(i)._1 == xy(i-1)._1)) map {xy(_)}

val keys = xy map {_._1} distinct

val groups = xy.groupBy(_._1)

keys.take(165) foreach {
k => 
val kdivs = List.range(2,k/2).filter(k % _ == 0).filter(d => x.indexOf(d)>=0)
val nonprimes = kdivs map {d => k -> xy(x.indexOf(d))._2*(k/d)}
val g = groups(k) ::: nonprimes
//println("k = " + k + " g = " + g)
val gl = g.length
val r = for (i <- List.range(0,gl); j <- List.range(i+1,gl); 
     val b2 = k*k; 
     val gj2 = g(j)._2*g(j)._2;
     val gi2 = g(i)._2*g(i)._2;
     val e2 = math.max(gi2, gj2);
     val f2 = math.min(gi2, gj2);
     val a2 = b2 + e2 + f2; 
     if (isPS(a2))) yield (a2, b2, b2+e2, b2+f2, e2, f2, (a2+b2+2*e2)/2,k)
if (r!=List()) println("r = " + r)
//r map {ri => genXYZ2(ri(0), ri(1), ri(2), ri(3), ri(4), ri(5))}
}

def genXYZ2(a2: Long, b2: Long, c2: Long, d2: Long, e2: Long, f2: Long) = {
val x = (a2+b2)/2
val y = (e2+f2)/2
val z = (c2+d2)/2
(x,y,z)
}


def genXYZ(a: Long, b: Long, c: Long, d: Long, e: Long, f: Long) = {
val x = (a*a+b*b)/2
val y = (e*e+f*f)/2
val z = (c*c+d*d)/2
println("(x,y,z) = " + (x,y,z) + " s = " + (x+y+z))
}


def isPalin(i: Long) = {
val si = i.toString
//val sin = si.length
//List.range(0,sin/2+1).filter(p => si(p) != si(sin-1-p)) == List()
si == si.reverse
}

isPalin(353)

def solP125(max: Int) = {
val maxp = math.sqrt(max).toInt
val aux = for(i <- List.range(1,maxp); k <- List.range(i+1,maxp); val n = sumSquares(i,k); if (isPalin(n))) yield (n,i,k)
//val sols1 = aux.filter(e => check(e._2,e._3,e._1))
//val sols2 = aux.filter(e => !check(e._2,e._3,e._1))
//val sols = (sols1 map {_._1}).distinct.filter(_ < max)
//println("sols1 = " + sols1.sorted + " l1 = " + sols1.length + "\nsols2 = " + sols2 + " l2 = " + sols2.length + "\nsols = " + sols + " l = " + sols.length) 
//sols.foldLeft(0L)(_+_)
(aux map {_._1}).distinct.filter(_ < max).foldLeft(0L)(_+_)
}

solP125(1000)

solP125(100000000)

def check(i: Int, k: Int, r: Long) = {
List.range(i,k+1).foldLeft(0L)((r,c)=>(c*c+r)) == r
}

def sumSquares(i: Int, k: Int) = {
    (k*(k+1L)*(2*k+1L) - (i-1)*i*(2*i-1))
}

//decompose 10^k into a sum \sigma 2^{i_j} with i_j distinct
def dec2(k: Int) : List[Int] = { 
if (k==1) List(1,3) 
else {
if (k % 2 == 0) {
   val r = dec2(k/2)
   simpl(mult(r,r).sorted)
}
else {
     val r1 = dec2(k/2)
     val r2 = dec2(k - k/2)
     simpl(mult(r1,r2).sorted)
}
}
}

//l1 and l2 are the lists of exponents of 2
def mult(l1: List[Int], l2: List[Int]) = {
val n1 = l1.length
val n2 = l2.length
val range1 = List.range(0,n1)
val range2 = List.range(0,n2)
for(i1 <- range1; i2 <- range2) yield l1(i1)+l2(i2)
}

//simplifies a sorted list of exponents of 2, i.e., each 2 equal exponents p are replaced by 1 exp p+1 until all exp are distinct
def simpl(l: List[Int]) : List[Int] = {
println(" l = " + l)
l match {
case h1::h2::tail => { println("h1 = " + h1 + " h2 = " + h2 + " tail = " + tail); if (h1 == h2) simpl(((h1+1)::tail).sorted) else h1::simpl(h2::tail) }
case nil => { println("nil"); nil }
}
}

//check if 10^k = \sum 2^l(i)
def check(k: Int, l: List[Int]) = math.pow(10,k).toLong == l.foldLeft(0L)((r,c)=>(r+math.pow(2,c).toLong))

List.range(1,15).filter(k => !check(k, dec2(k)))

/*counts the nb of equiv lists:
* (1,3), (0,0,3), (1,2,2), (0,0,2,2), (0,0,1,1,2)
* n(2,5,6) = n(1,5,6) + // for 2 = 1,1
*            n(4,6) + // for 5 = 4,4
* we can't rewrite 6 as 5,5 because of the prec 5 (it'd lead to 3 occurences of 5)
*/
def countAll(l: List[Int], prec: Int, c: Int, r: Long) : Long = {
println("c = " + c + " l = " + l + " prec = " + prec)
if (c > 100) 0L
else {
val n = l.length
if (n == 1) if (l(0) - 1 > prec) (r+l(0)-1) else r
else {
     val nl0 = if (l(0) == 1) l.tail else (l(0)-1)::l.tail
     println()
     val sol1 = countAll(nl0, -1,c+1, r+1) else countAll(nl0, l(0),c+1, r+1)
     val sol = List.range(1,n) map {
          i =>
	    val cv = l(i) 
            val nl = l.drop(i+1)
	    if (cv - 1 > l(i-1)) 
	       if (cv==1) countAll(nl, -1,c+1,r+1) else countAll((cv-1)::nl, cv,c+1,r+1)
	    else 0
     }
     sol.foldLeft(0L)(_+_) + sol1
     }
}
}


countAll(List(1,3),-1,0,0)

countAll(List(2,5,6),-1,0,0)


//def countAll(l: List[Int], cpos: Int, c: Int, all: Set[List[Int]]) : Long = {
def countAll(l: List[Int], cpos: Int, c: Int, all: Set[List[Int]]) : Set[List[Int]] = {
//println(" l = " + l + " cpos = " + cpos + " c = " + c)
val n = l.length
//if (c > 10) Set(List(-10000000))
//else {
if (cpos == (n-1)) {   
   //println("all = " + all)
   val diff = (l(cpos) - 1 - l(cpos-1)) 
   //if (diff > 0) diff else 0
   //Set()
   all
}
else {
	val cand = List.range(cpos,n).filter(p => (p==0 && l(0)!=0) || (p>0 && l(p) > (l(p-1) + 1)))
	//println("cand = " + cand)
	if (cand == List()) countAll(l, cpos+1,c, all)
	else {
	        val sol = cand map {
		          i =>
			  	    val cv = l(i) 
				    val left = l.drop(i+1)
 	                            val right = l.take(i)
 	                            val nl = right:::List(cv-1,cv-1):::left
 	                            countAll(nl, cpos, c + 1, all+nl) 		
				         }
				    sol.foldLeft(Set[List[Int]]())(_++_) 
      }//else
     }
}
//}

val r = countAll(List(1,3),0,0, Set())

val r2 = countAll(List(2,5,6),0,0, Set())

val r3 = countAll(dec2(3),0,0, Set())

def countAllE(l: List[Int], prev: Int, cpos: Int, r: Long, c: Int) : Long = {
if (c < 10) println(" l = " + l + " cpos = " + cpos + " prev = " + prev + " c = " + c + " r = " + r)
val n = l.length
if (c > 10) -10000000
else {
//if (cpos == (n-1)) {  
if (n == 1) {   
   //println("all = " + all)
   val diff = (l(0) - prev - 1)
   if (diff > 0) r + diff else r
}
else {
	val cand = List.range(0,n).filter(p => (p==0 && l(0)>(prev + 1)) || (p>0 && l(p) > (l(p-1) + 1)))
	println("cand = " + cand)
	if (cand == List()) countAllE(l.drop(cpos+1), l(cpos)+1, 0, r, c)
	else {
	        val sol = cand map {
		          i =>
			  	    val cv = l(i) 
				    val nprev = if (i==0) (prev) else (l(i-1))
  				    println("cv = " + cv + " nprev = " + nprev)
 	                            if (cv == (nprev + 1)) {
				        countAllE(l.drop(2), cv, 0, 1, c + 1)
					}
				    else {
				        val nl = (cv-1)::(cv-1)::l.drop(i+1)				    				    
 	                                countAllE(nl, nprev, 0, r + 1, c + 1) 
					}		
				    }
				    sol.foldLeft(0L)(_+_) 
      }//else
     }
}
}

val r = countAllE(dec2(1),-1,0,0,0)

val r2 = countAllE(dec2(2),-1,0,0,0)

val r3 = countAll(dec2(3),-1,0,0,0)


def countAllF(l: List[Int], prev: Int, c: Int) : Long = {
//if (c < 10) println(" l = " + l + " prev = " + prev + " c = " + c)
val n = l.length
if (c > 70) { println("c 51 reached"); -10000000 }
else {
if (n == 1) {   
   val diff = (l(0) - prev - 1L)
   if (diff > 0) diff else 0L
}
else {
	val cand = List.range(0,n).filter(p => (p==0 && l(0)>(prev + 1)) || (p>0 && l(p) > (l(p-1) + 1)))
	//println("cand = " + cand)
	if (cand == List()) 0L 
/*	else if (cand.length == 1) {	     
	     val r = l(cand(0)) - 1L - (if (cand(0)==0) (prev) else (l(cand(0)-1)))
	     //println("ret " + r)	
	     r
	}
*/
	else {
	        val sol = cand map {
		          i =>
			  	    val cv = l(i) 
				    val nprev = if (i==0) (prev) else (l(i-1))
  				    //println("cv = " + cv + " nprev = " + nprev)
				    val nl = (cv-1)::(cv-1)::l.drop(i+1)				    				    
 	                            1L + countAllF(nl, nprev, c + 1) 
				    }
				    sol.foldLeft(0L)(_+_) 
      }//else
     }
}
}

val r = countAllF(dec2(1),-1,0)

val r2 = countAllF(dec2(2),-1,0)

def solP169(exp10: Int) = {
val r = dec2(exp10)
}

2^25 + 2^27 + 2^30 + 2^35 + 2^38 + 2^40 + 2^50 + 2^52 + 2^57 + 2^58 + 2^60 + 2^64 + 2^66 + 2^68 + 2^71 + 2^72 + 2^74 + 2^78 + 2^83 - 10^25 == 0


	       def genPerm(n: Int) : List[List[Int]] = {
		 if (n==2) List(List(1,2), List(2,1))
		 else {
		   val indices = List.range(0,n)
		   val p = genPerm(n-1)
		   val p2 = p map {
		     pi => 
		       indices map {
			 i => 
			   (pi.take(i):+n):::pi.drop(i)
		       }
		   }
		   p2.flatten
		 }
	       }


val l = List((2,Set(3,4,5)), (1,Set(2,3)), (2, Set(4,5)), (1,Set(2,3,4)))

def simplSetWrong(s: List[Pair[Int,Set[Int]]], sr:  List[Pair[Int,Set[Int]]]) :  List[Pair[Int,Set[Int]]] = {
val ls = s.sortBy(_._1).sort(_._2 subsetOf _._2) 
//println("ls " + ls + " sr = " + sr)
ls match {
  case h1::h2::tail => if ((h1._1 == h2._1) && (h1._2 subsetOf h2._2)) simplSet(ls.tail, ls.tail) else simplSet(tail, ls)
  case h => sr
}
}


//foldLeft(List[Pair[Int,Set[Int]]]())((r,c)=>(if (r.length==0) else r.head() ))
  def simplSet(s: List[Pair[Int,Set[Int]]]) = {
    val keys = s map { _._1 }
    var sc = s
    val res = keys map { 
      k => 
	val r = sc.partition(_._1 == k) 
      sc = r._2      
      //println("r1 = " + r._1)
      val group = r._1 map {_._2}
      if (group.size < 2) r._1
      else r._1.filter(saux => !(group.exists(el => (saux._2 subsetOf el) && saux._2.size < el.size)))     
    }
    //println("res = " + res)
    res.flatten
  }

simplSet(List((1,Set(1))))

  def genSets(s: List[Pair[Int,Set[Int]]], b: Int) = {
    val s2 = s.partition(_._1 == b)
    println("s2 = " + s2)
    val sb = s2._1
    val ns = sb map {
      a => 
	val a1 = a._2 - b
      a1 map {ai => (ai, a1)}
    }
    val res = ns.flatten:::s2._2
    simplSet(res)
    /*
     val r1 = simplSet(s2._2, List())
     val r2 = simplSet(ns.flatten, List())    
     if (r1.length < r2.length) r1:::r2 else r2:::r1
     */ 
  }

  //def genSetsLF(s: List[Pair[Int,Set[Int]]], b: List[Int]): List[Pair[Int,Set[Int]]] = b.foldLeft(s)((r, c)=> (genSets(r, b.head)))

  def genSetsL(s: List[Pair[Int,Set[Int]]], b: List[Int]): List[Pair[Int,Set[Int]]] = {
    if (b.length == 0) s
    else {
      val l = genSets(s, b.head)
      println("s = " + s + " b0  = " + b.head + " l = " + l)
      genSetsL(l, b.tail)
    }
  }

val l2 = List( (0,Set(0,1,2,3)) )

val b = List(0,1,2,1,3,1,2,1)

def solP166() = {

val digits = List.range(0,10)

val res = 
for (a11 <- digits; a12 <- digits; a13 <- digits; a14 <- digits; 
     a21 <- digits; a22 <- digits; a23 <- digits; a24 <- digits; 
     a31 <- digits; a32 <- digits; a33 <- digits; a34 <- digits; 
     a41 <- digits; a42 <- digits; a43 <- digits; a44 <- digits; 
     if ((a11 + a12 + a13 + a14) == (a21 + a22 + a23 + a24) && (a21 + a22 + a23 + a24) == (a31 + a32 + a33 + a34) && 
	 (a31 + a32 + a33 + a34) == (a41 + a42 + a43 + a44) && 
	 (a11 + a21 + a31 + a41) == (a12 + a22 + a32 + a42) && (a12 + a22 + a32 + a42) == (a13 + a23 + a33 + a43) &&
	 (a13 + a23 + a33 + a43) == (a14 + a24 + a34 + a44) && 
	 (a11 + a22 + a33 + a44) == (a11 + a12 + a13 + a14) &&
	 (a14 + a23 + a32 + a41) == (a11 + a12 + a13 + a14)) ) yield 1

res.length

}

def solP166Old(s: Int) = {

//def min(x: Int, y: Int) = math.min(x,y).toInt
def min2(x: Int, y: Int) = math.min(x,y).toInt
def min(x: Int, y: Int, z: Int): Int = min2(min2(x,y), z)

val res = 
for (a11 <- List.range(0, s + 1); 
     a12 <- List.range(0, s + 1 - a11); 
     a13 <- List.range(0, s + 1 - a11 - a12); 
     val a14 = s - a11 - a12 - a13; 
     
     a21 <- List.range(0, s + 1 - a11); 
     a22 <- List.range(0, 1 + min(s - a12, s - a21, s - a11)); 
     a23 <- List.range(0, 1 + min(s - a21 - a22, s - a13, s - a14)); 
     val a24 = s - a21 - a22 - a23; 

     a31 <- List.range(0, 1 + s - a11 - a21); 
     a32 <- List.range(0, 1 + min(s - a31, s - a12 - a22, s - a14 - a23)); 
     a33 <- List.range(0, 1 + min(s - a31 - a32, s - a13 - a23, s - a11 - a22)); 
     val a34 = s - a31 - a32 - a33;

     val a41 = s - a11 - a21 - a31; 
     val a42 = s - a12 - a22 - a32; 
     val a43 = s - a13 - a23 - a33;

     if ( (a11 + a21 + a31) == (a14 + a23 + a32) && 
	  (a11 + a22 + a33) == (a41 + a42 + a43) && 
	  (a11 + a22 + a33) == (a14 + a24 + a34))
     //no need of a44
) yield (a11 + " " + a12  + " " + a13 + " " + a14 + "\n" +  a21 + " " + a22 + " " + a23 + " " + a24 + "\n" + a31 + " " + a32 + " " + a33 + " " + a34 + "\n" +  a41 + " " + a42 + " " + a43 + "\n")

println(res.foldLeft("")(_+ "\n" +_))

res.length

}

def solP166Aux(s: Int, maxD: Int) = {

def min2(x: Int, y: Int) = math.min(x,y).toInt

def min(x: Int, y: Int, z: Int): Int = min2(min2(x,y), z)

val lines1 = 
for (a11 <- List.range(0, 1 + min2(maxD, s)); 
     a12 <- List.range(0, 1 + min2(maxD, s - a11)); 
     a13 <- List.range(0, 1 + min2(maxD, s - a11 - a12)); 
     val a14 = s - a11 - a12 - a13; 
     if (a14 <= maxD)) yield List(a11, a12, a13, a14)

val lines2 = 
for (l <- lines1; 
     a21 <- List.range(0, 1 + min2(maxD, s - l(0))); 
     a22 <- List.range(0, 1 + min2(maxD, min(s - l(1), s - a21, s - l(0)))); 
     a23 <- List.range(0, 1 + min2(maxD, min(s - a21 - a22, s - l(2), s - l(3)))); 
     val a24 = s - a21 - a22 - a23; 
     if (a24 <= maxD)) yield l:+a21:+a22:+a23:+a24

val lines3 = 
for (l <- lines2;
     a31 <- List.range(0, 1 + min2(maxD, s - l(0) - l(4))); 
     a32 <- List.range(0, 1 + min2(maxD, min(s - a31, s - l(1) - l(5), s - l(3) - l(6)))); 
     a33 <- List.range(0, 1 + min2(maxD, min(s - a31 - a32, s - l(2) - l(6), s - l(0) - l(5)))); 
     val a34 = s - a31 - a32 - a33;
     if ( (a34 <= maxD) && 
	  (l(0) + l(4) + a31) == (l(3) + l(6) + a32) &&
	  (l(0) + l(5) + a33) == (l(3) + l(7) + a34) ) ) yield l:+a31:+a32:+a33:+a34   
val res =
for (l <- lines3;
     val a41 = s - l(0) - l(4) - l(8); 
     val a42 = s - l(1) - l(5) - l(9); 
     val a43 = s - l(2) - l(6) - l(10);
     val a44 = s - l(3) - l(7) - l(11);
     if ( (a41 <= maxD) && (a42 <= maxD) && (a43 <= maxD) && (a44 <= maxD) &&
	  (l(0) + l(5) + l(10)) == (a41 + a42 + a43) ) 
) yield l:+a41:+a42:+a43:+a44
res.length
}

def solP166 = List.range(0, 18).foldLeft(0L)(_+solP166Aux(_,9))*2 + solP166Aux(18,9)

//5656546

def solP166 = List.range(0, 37).foldLeft(0L)(_+solP166Aux(_,9))

solP166Aux(8,2)

def checkP166(s: Int, i: Int) = {
val maxS = i*4
val r1 = solP166Aux(s, i) 
val r2 = solP166Aux(maxS - s, i) 
val r2C = r2 map {l => l map {li => i - li} }
r1.toSet -- r2C.toSet
}

def solP166 = List.range(0, 17).foldLeft(0L)(_+solP166Aux(_,9).length)*2 + solP166Aux(18,9).length


     if ((a11 + a12 + a13 + a14) == (a21 + a22 + a23 + a24) && (a21 + a22 + a23 + a24) == (a31 + a32 + a33 + a34) && 
	 (a31 + a32 + a33 + a34) == (a41 + a42 + a43 + a44) && 
	 (a11 + a21 + a31 + a41) == (a12 + a22 + a32 + a42) && (a12 + a22 + a32 + a42) == (a13 + a23 + a33 + a43) &&
	 (a13 + a23 + a33 + a43) == (a14 + a24 + a34 + a44) && 
	 (a11 + a22 + a33 + a44) == (a11 + a12 + a13 + a14) &&
	 (a14 + a23 + a32 + a41) == (a11 + a12 + a13 + a14)) 

def solP166(a34: Int) = {

val digits = List.range(0,10)

val resAux1 =  
for (a12 <- digits; a13 <- digits; a14 <- digits; 
     a21 <- digits; a22 <- digits; 
     a31 <- digits; a33 <- digits; 
     a41 <- digits; a44 <- digits; 
     if ((a12 + a13 + a14) == (a21 + a31 + a41) && 
	 (a21 + a31 + a41) == (a22 + a33 + a44)) ) yield List(a12,a13,a14,a21,a22,a31,a33,a41,a44)

//r0 12, r1 13, r2 14, r3 21, r4 22, r5 31, r6 33, r7 41, r8 44
//val resAux11 = resAux1.take(10)
val resAux2 = 
for (r <- resAux1; 
     val a42 = a34 + (r(1) + r(2) + r(6) - r(3) - r(7) - r(4)); 
     val a23 = (r(7) + a42 + r(8)) - (r(1) + r(6));
     if (r(3) + r(4) + a23 == r(2) + a34 + r(8) && 
	 r(2) + a23 + r(8) == r(0) + r(4) + a42)        
) yield 10 - (r:+a23:+a34:+a42).max

val n = resAux2.length
val nc = resAux2.foldLeft(0L)(_+_)
println(" n = " + n + " s4 = " + nc)
nc
}

  def diffs(x: List[Int], i: List[Char], r: Int) : Int = {
    if (x.length == 1) if (x(0) == i(0) - 48) (r + 1) 
		       else r
    else diffs(x.tail, i.tail, (if (x(0) == i(0) - 48) (r + 1) else r)) 
  }

def mindNb = {
val digits = List.range(0,10)

val digits1 = List.range(0,7):+8:+9
val digits2 = List.range(1,10)
val digits3 = digits1
val digits4 = List.range(0,9)
val digits5 = 0+:1+:2+:3+:List.range(5,10)

for(a1 <- digits1; a2 <- digits2; a3 <- digits3; a4 <- digits4; a5 <- digits5;
    val x = List(a1,a2,a3,a4,a5);
    if ( (diffs(x, 90342.toString.toList, 0) == 2) && 
	 (diffs(x, 39458.toString.toList, 0) == 2) &&
	 (diffs(x, 34109.toString.toList, 0) == 1) &&
	 (diffs(x, 51545.toString.toList, 0) == 2) &&
	 (diffs(x, 12531.toString.toList, 0) == 1) )) yield x       
}

def commonPos(s1: List[Int], s2: List[Int]) = for (i <- List.range(0,s1.length); if(s1(i) == s2(i))) yield (i, s1(i))

def mindNb(i: Int) = {

val allDiff = "2321386104303845".toList map {_.toInt - 48}

val aux = List(("5616185650518293", 2), ("3847439647293047", 1), ("5855462940810587", 3), 
	       ("4513559094146117", 2), ("3174248439465858", 1), ("4296849643607543", 3), 
	       ("2615250744386899", 2), ("8157356344118483", 1), ("8690095851526254", 3), 
	       ("6442889055042768", 2), ("6913859173121360", 1), ("3041631117224635", 3),
	       ("5251583379644322", 2), ("4895722652190306", 1), ("1748270476758276", 3), 
	       ("2659862637316867", 2), ("6375711915077050", 1), ("1841236454324589", 3),
	       ("2326509471271448", 2),                          ("7890971548908067", 3),  
								 ("9742855507068353", 3)
	     )

val matchesL = aux map {p => ((p._1.toList map {el => el - 48}), p._2)}

val parts = matchesL.partition(_._2!=1)
val matchesLWithout1 = parts._1
val matchesLOnly1 = parts._2

//println("matches-1 = " + matchesLWithout1)

var magicN = List(-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1)

val possib =  List.range(0,aux.length)

//toSet.toList to delete duplicates
val candPairs = (for (i <- possib; val cand = matchesL(i); p2 <- matchesL.drop(i+1)) yield commonPos(cand._1, p2._1)).flatten.filter(_!=List()).toSet.toList

val n = candPairs.length

val candD = List(
candPairs.filter(_._1 == 0) map {_._2} filter(_!=allDiff(0)),
candPairs.filter(_._1 == 1) map {_._2} filter(_!=allDiff(1)),
candPairs.filter(_._1 == 2) map {_._2} filter(_!=allDiff(2)), 
candPairs.filter(_._1 == 3) map {_._2} filter(_!=allDiff(3)),
candPairs.filter(_._1 == 4) map {_._2} filter(_!=allDiff(4)),
candPairs.filter(_._1 == 5) map {_._2} filter(_!=allDiff(5)),
candPairs.filter(_._1 == 6) map {_._2} filter(_!=allDiff(6)),
candPairs.filter(_._1 == 7) map {_._2} filter(_!=allDiff(7)),
candPairs.filter(_._1 == 8) map {_._2} filter(_!=allDiff(8)),
candPairs.filter(_._1 == 9) map {_._2} filter(_!=allDiff(9)),
candPairs.filter(_._1 == 10) map {_._2} filter(_!=allDiff(10)),
candPairs.filter(_._1 == 11) map {_._2} filter(_!=allDiff(11)),
candPairs.filter(_._1 == 12) map {_._2} filter(_!=allDiff(12)),
candPairs.filter(_._1 == 13) map {_._2} filter(_!=allDiff(13)),
candPairs.filter(_._1 == 14) map {_._2} filter(_!=allDiff(14)),
candPairs.filter(_._1 == 15) map {_._2} filter(_!=allDiff(15)))

//println("n = " + n + " cand = " + candD.foldLeft("")(_+"\n"+_))

val start = System.currentTimeMillis

val res = 
for(a0 <- candD(0); 
    val diffa0Fixed = matchesLOnly1.filter(p => (p._1(0) == a0));
    a1 <- candD(1).take(i).toSet -- (diffa0Fixed map {_._1(1)}); 
    val diffa1Fixed = matchesLOnly1.filter(p => (p._1(1) == a1));
    a2 <- candD(2).take(i).toSet -- (diffa0Fixed map {_._1(2)}) -- (diffa1Fixed map {_._1(2)}) ;  
    val diffa2Fixed = matchesLOnly1.filter(p => (p._1(2) == a2));
    a3 <- candD(3).take(i).toSet -- (diffa0Fixed map {_._1(3)}) -- (diffa1Fixed map {_._1(3)}) -- (diffa2Fixed map {_._1(3)});  
    val diffa3Fixed = matchesLOnly1.filter(p => (p._1(3) == a3));
    a4 <- candD(4).take(i).toSet -- (diffa0Fixed map {_._1(4)}) -- (diffa1Fixed map {_._1(4)}) -- (diffa2Fixed map {_._1(4)}) -- (diffa3Fixed map {_._1(4)}); 
    val diffa4Fixed = matchesLOnly1.filter(p => (p._1(4) == a4));
    a5 <- candD(5).take(i).toSet -- (diffa0Fixed map {_._1(5)}) -- (diffa1Fixed map {_._1(5)}) -- 
				    (diffa2Fixed map {_._1(5)}) -- (diffa3Fixed map {_._1(5)}) --
				    (diffa4Fixed map {_._1(5)});
    val diffa5Fixed = matchesLOnly1.filter(p => (p._1(5) == a5));
    a6 <- candD(6).take(i).toSet -- (diffa0Fixed map {_._1(6)}) -- (diffa1Fixed map {_._1(6)}) -- 
				    (diffa2Fixed map {_._1(6)}) -- (diffa3Fixed map {_._1(6)}) --
				    (diffa4Fixed map {_._1(6)}) -- (diffa5Fixed map {_._1(6)});
    val diffa6Fixed = matchesLOnly1.filter(p => (p._1(6) == a6));
    a7 <- candD(7).take(i).toSet -- (diffa0Fixed map {_._1(7)}) -- (diffa1Fixed map {_._1(7)}) -- 
				    (diffa2Fixed map {_._1(7)}) -- (diffa3Fixed map {_._1(7)}) --
				    (diffa4Fixed map {_._1(7)}) -- (diffa5Fixed map {_._1(7)}) --
				    (diffa6Fixed map {_._1(7)});
    val diffa7Fixed = matchesLOnly1.filter(p => (p._1(7) == a7));
    a8 <- candD(8).take(i).toSet -- (diffa0Fixed map {_._1(8)}) -- (diffa1Fixed map {_._1(8)}) -- 
				    (diffa2Fixed map {_._1(8)}) -- (diffa3Fixed map {_._1(8)}) --
				    (diffa4Fixed map {_._1(8)}) -- (diffa5Fixed map {_._1(8)}) --
				    (diffa6Fixed map {_._1(8)}) -- (diffa7Fixed map {_._1(8)});
    val diffa8Fixed = matchesLOnly1.filter(p => (p._1(8) == a8));
    a9 <- candD(9).take(i).toSet -- (diffa0Fixed map {_._1(9)}) -- (diffa1Fixed map {_._1(9)}) -- 
				    (diffa2Fixed map {_._1(9)}) -- (diffa3Fixed map {_._1(9)}) --
				    (diffa4Fixed map {_._1(9)}) -- (diffa5Fixed map {_._1(9)}) --
				    (diffa6Fixed map {_._1(9)}) -- (diffa7Fixed map {_._1(9)}) --
				    (diffa8Fixed map {_._1(9)});
    val diffa9Fixed = matchesLOnly1.filter(p => (p._1(9) == a9));
    a10 <- candD(10).take(i).toSet -- (diffa0Fixed map {_._1(10)}) -- (diffa1Fixed map {_._1(10)}) -- 
   				      (diffa2Fixed map {_._1(10)}) -- (diffa3Fixed map {_._1(10)}) --
				      (diffa4Fixed map {_._1(10)}) -- (diffa5Fixed map {_._1(10)}) --
				      (diffa6Fixed map {_._1(10)}) -- (diffa7Fixed map {_._1(10)}) --
				      (diffa8Fixed map {_._1(10)}) -- (diffa9Fixed map {_._1(10)});
    val diffa10Fixed = matchesLOnly1.filter(p => (p._1(10) == a10));
    a11 <- candD(11).take(i).toSet -- (diffa0Fixed map {_._1(11)}) -- (diffa1Fixed map {_._1(11)}) -- 
   				      (diffa2Fixed map {_._1(11)}) -- (diffa3Fixed map {_._1(11)}) --
				      (diffa4Fixed map {_._1(11)}) -- (diffa5Fixed map {_._1(11)}) --
				      (diffa6Fixed map {_._1(11)}) -- (diffa7Fixed map {_._1(11)}) --
				      (diffa8Fixed map {_._1(11)}) -- (diffa9Fixed map {_._1(11)}) --
				      (diffa10Fixed map {_._1(11)});
    val diffa11Fixed = matchesLOnly1.filter(p => (p._1(11) == a11));
    a12 <- candD(12).take(i).toSet -- (diffa0Fixed map {_._1(12)}) -- (diffa1Fixed map {_._1(12)}) -- 
   				       (diffa2Fixed map {_._1(12)}) -- (diffa3Fixed map {_._1(12)}) --
				       (diffa4Fixed map {_._1(12)}) -- (diffa5Fixed map {_._1(12)}) --
				       (diffa6Fixed map {_._1(12)}) -- (diffa7Fixed map {_._1(12)}) --
				       (diffa8Fixed map {_._1(12)}) -- (diffa9Fixed map {_._1(12)}) --
				       (diffa10Fixed map {_._1(12)}) -- (diffa11Fixed map {_._1(12)});
    val diffa12Fixed = matchesLOnly1.filter(p => (p._1(12) == a12));
    a13 <- candD(13).take(i).toSet -- (diffa0Fixed map {_._1(13)}) -- (diffa1Fixed map {_._1(13)}) -- 
   				       (diffa2Fixed map {_._1(13)}) -- (diffa3Fixed map {_._1(13)}) --
				       (diffa4Fixed map {_._1(13)}) -- (diffa5Fixed map {_._1(13)}) --
				       (diffa6Fixed map {_._1(13)}) -- (diffa7Fixed map {_._1(13)}) --
				       (diffa8Fixed map {_._1(13)}) -- (diffa9Fixed map {_._1(13)}) --
				       (diffa10Fixed map {_._1(13)}) -- (diffa11Fixed map {_._1(13)}) --
				       (diffa12Fixed map {_._1(13)});
    val diffa13Fixed = matchesLOnly1.filter(p => (p._1(13) == a13));
    a14 <- candD(14).take(i).toSet -- (diffa0Fixed map {_._1(14)}) -- (diffa1Fixed map {_._1(14)}) -- 
   				       (diffa2Fixed map {_._1(14)}) -- (diffa3Fixed map {_._1(14)}) --
				       (diffa4Fixed map {_._1(14)}) -- (diffa5Fixed map {_._1(14)}) --
				       (diffa6Fixed map {_._1(14)}) -- (diffa7Fixed map {_._1(14)}) --
				       (diffa8Fixed map {_._1(14)}) -- (diffa9Fixed map {_._1(14)}) --
				       (diffa10Fixed map {_._1(14)}) -- (diffa11Fixed map {_._1(14)}) --
				       (diffa12Fixed map {_._1(14)}) -- (diffa13Fixed map {_._1(14)});
    val diffa14Fixed = matchesLOnly1.filter(p => (p._1(14) == a14));
    a15 <- candD(15).take(i).toSet -- (diffa0Fixed map {_._1(15)}) -- (diffa1Fixed map {_._1(15)}) -- 
   				       (diffa2Fixed map {_._1(15)}) -- (diffa3Fixed map {_._1(15)}) --
				       (diffa4Fixed map {_._1(15)}) -- (diffa5Fixed map {_._1(15)}) --
				       (diffa6Fixed map {_._1(15)}) -- (diffa7Fixed map {_._1(15)}) --
				       (diffa8Fixed map {_._1(15)}) -- (diffa9Fixed map {_._1(15)}) --
				       (diffa10Fixed map {_._1(15)}) -- (diffa11Fixed map {_._1(15)}) --
				       (diffa12Fixed map {_._1(15)}) -- (diffa13Fixed map {_._1(15)}) --
				       (diffa14Fixed map {_._1(15)});
    val x = List(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15);
    if (matchesLWithout1.take(2).foldLeft(true)((r,c) => (r && (commonPos(x, c._1).length == c._2)) ))
) yield x

val end = System.currentTimeMillis

print("Took " + (end -start)/1000.0 + " s.")

res

}

def mindNb = {
val digits = List.range(0,10)

val digits1 = List.range(0,7):+8:+9
val digits2 = List.range(1,10)
val digits3 = digits1
val digits4 = List.range(0,9)
val digits5 = 0+:1+:2+:3+:List.range(5,10)

for(a1 <- digits1; a2 <- digits2; a3 <- digits3; a4 <- digits4; a5 <- digits5;
    val x = List(a1,a2,a3,a4,a5);
    if ( (diffs(x, 90342.toString.toList, 0) == 2) && 
	 (diffs(x, 39458.toString.toList, 0) == 2) &&
	 (diffs(x, 34109.toString.toList, 0) == 1) &&
	 (diffs(x, 51545.toString.toList, 0) == 2) &&
	 (diffs(x, 12531.toString.toList, 0) == 1) )) yield x       
}

def commonPos(s1: List[Int], s2: List[Int]) = for (i <- List.range(0,s1.length); if(s1(i) == s2(i))) yield (i, s1(i))

def mindNb(i: Int) = {
val digits = List.range(0,10)
val allDiff = "2321386104303845".toList map {_.toInt - 48}

val aux = List(("5616185650518293", 2), ("3847439647293047", 1), ("5855462940810587", 3), 
	       ("4513559094146117", 2), ("3174248439465858", 1), ("4296849643607543", 3), 
	       ("2615250744386899", 2), ("8157356344118483", 1), ("8690095851526254", 3), 
	       ("6442889055042768", 2), ("6913859173121360", 1), ("3041631117224635", 3),
	       ("5251583379644322", 2), ("4895722652190306", 1), ("1748270476758276", 3), 
	       ("2659862637316867", 2), ("6375711915077050", 1), ("1841236454324589", 3),
	       ("2326509471271448", 2),                          ("7890971548908067", 3),  
								 ("9742855507068353", 3)
	     )

val matchesL = aux map {p => ((p._1.toList map {el => el - 48}), p._2)}

val parts = matchesL.partition(_._2!=1)
val matchesLWithout1 = parts._1
val matchesLOnly1 = parts._2
val parts2 = matchesLWithout1.partition(_._2==2)
val matchesLOnly2 = parts2._1
val matchesLOnly3 = parts2._2

//println("matches-1 = " + matchesLWithout1)

var magicN = List(-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1)

//val possib =  List.range(0,aux.length)
//toSet.toList to delete duplicates
//val candPairs = (for (i <- possib; val cand = matchesLOnly3(i); p2 <- matchesL.drop(i+1)) yield commonPos(cand._1, p2._1)).flatten.filter(_!=List()).toSet.toList

val possib =  List.range(0,matchesLOnly3.length)
val candPairs = (for (i <- possib; val cand = matchesLOnly3(i); p2 <- matchesLOnly3.drop(i+1); val c = commonPos(cand._1, p2._1); if (c!=List())) yield c).flatten.toSet.toList

val n = candPairs.length

val candD = List(
candPairs.filter(_._1 == 0) map {_._2} filter(_!=allDiff(0)),
candPairs.filter(_._1 == 1) map {_._2} filter(_!=allDiff(1)),
candPairs.filter(_._1 == 2) map {_._2} filter(_!=allDiff(2)), 
candPairs.filter(_._1 == 3) map {_._2} filter(_!=allDiff(3)),
candPairs.filter(_._1 == 4) map {_._2} filter(_!=allDiff(4)),
candPairs.filter(_._1 == 5) map {_._2} filter(_!=allDiff(5)),
candPairs.filter(_._1 == 6) map {_._2} filter(_!=allDiff(6)),
candPairs.filter(_._1 == 7) map {_._2} filter(_!=allDiff(7)),
//candPairs.filter(_._1 == 8) map {_._2} filter(_!=allDiff(8)),
digits.filter(_!=allDiff(8)),
//candPairs.filter(_._1 == 9) map {_._2} filter(_!=allDiff(9)),
digits.filter(_!=allDiff(9)),
//candPairs.filter(_._1 == 10) map {_._2} filter(_!=allDiff(10)),
digits.filter(_!=allDiff(10)),
//candPairs.filter(_._1 == 11) map {_._2} filter(_!=allDiff(11)),
digits.filter(_!=allDiff(11)),
//candPairs.filter(_._1 == 12) map {_._2} filter(_!=allDiff(12)),
digits.filter(_!=allDiff(12)),
//candPairs.filter(_._1 == 13) map {_._2} filter(_!=allDiff(13)),
digits.filter(_!=allDiff(13)),
//candPairs.filter(_._1 == 14) map {_._2} filter(_!=allDiff(14)),
digits.filter(_!=allDiff(14)),
//candPairs.filter(_._1 == 15) map {_._2} filter(_!=allDiff(15))
digits.filter(_!=allDiff(15))
)

println("n = " + n + " cand = " + candD.foldLeft("")(_+"\n"+_))

val start = System.currentTimeMillis

val res = 
for(a0 <- List(1,6); 
    val diffa0Fixed = matchesLOnly1.filter(p => (p._1(0) == a0));
    a1 <- candD(1).take(i).toSet -- (diffa0Fixed map {_._1(1)}); 
    val diffa1Fixed = matchesLOnly1.filter(p => (p._1(1) == a1));
    a2 <- candD(2).take(i).toSet -- (diffa0Fixed map {_._1(2)}) -- (diffa1Fixed map {_._1(2)}) ;  
    val diffa2Fixed = matchesLOnly1.filter(p => (p._1(2) == a2));
    a3 <- candD(3).take(i).toSet -- (diffa0Fixed map {_._1(3)}) -- (diffa1Fixed map {_._1(3)}) -- (diffa2Fixed map {_._1(3)});  
    val diffa3Fixed = matchesLOnly1.filter(p => (p._1(3) == a3));
    a4 <- candD(4).take(i).toSet -- (diffa0Fixed map {_._1(4)}) -- (diffa1Fixed map {_._1(4)}) -- (diffa2Fixed map {_._1(4)}) -- (diffa3Fixed map {_._1(4)}); 
    val diffa4Fixed = matchesLOnly1.filter(p => (p._1(4) == a4));
    a5 <- candD(5).take(i).toSet -- (diffa0Fixed map {_._1(5)}) -- (diffa1Fixed map {_._1(5)}) -- 
				    (diffa2Fixed map {_._1(5)}) -- (diffa3Fixed map {_._1(5)}) --
				    (diffa4Fixed map {_._1(5)});
    val diffa5Fixed = matchesLOnly1.filter(p => (p._1(5) == a5));
    a6 <- candD(6).take(i).toSet -- (diffa0Fixed map {_._1(6)}) -- (diffa1Fixed map {_._1(6)}) -- 
				    (diffa2Fixed map {_._1(6)}) -- (diffa3Fixed map {_._1(6)}) --
				    (diffa4Fixed map {_._1(6)}) -- (diffa5Fixed map {_._1(6)});
    val diffa6Fixed = matchesLOnly1.filter(p => (p._1(6) == a6));
    a7 <- candD(7).take(i).toSet -- (diffa0Fixed map {_._1(7)}) -- (diffa1Fixed map {_._1(7)}) -- 
				    (diffa2Fixed map {_._1(7)}) -- (diffa3Fixed map {_._1(7)}) --
				    (diffa4Fixed map {_._1(7)}) -- (diffa5Fixed map {_._1(7)}) --
				    (diffa6Fixed map {_._1(7)});
    val diffa7Fixed = matchesLOnly1.filter(p => (p._1(7) == a7));
    a8 <- candD(8).take(i).toSet -- (diffa0Fixed map {_._1(8)}) -- (diffa1Fixed map {_._1(8)}) -- 
				    (diffa2Fixed map {_._1(8)}) -- (diffa3Fixed map {_._1(8)}) --
				    (diffa4Fixed map {_._1(8)}) -- (diffa5Fixed map {_._1(8)}) --
				    (diffa6Fixed map {_._1(8)}) -- (diffa7Fixed map {_._1(8)});
    val diffa8Fixed = matchesLOnly1.filter(p => (p._1(8) == a8));
    a9 <- candD(9).take(i).toSet -- (diffa0Fixed map {_._1(9)}) -- (diffa1Fixed map {_._1(9)}) -- 
				    (diffa2Fixed map {_._1(9)}) -- (diffa3Fixed map {_._1(9)}) --
				    (diffa4Fixed map {_._1(9)}) -- (diffa5Fixed map {_._1(9)}) --
				    (diffa6Fixed map {_._1(9)}) -- (diffa7Fixed map {_._1(9)}) --
				    (diffa8Fixed map {_._1(9)});
    val diffa9Fixed = matchesLOnly1.filter(p => (p._1(9) == a9));
    a10 <- candD(10).take(i).toSet -- (diffa0Fixed map {_._1(10)}) -- (diffa1Fixed map {_._1(10)}) -- 
   				      (diffa2Fixed map {_._1(10)}) -- (diffa3Fixed map {_._1(10)}) --
				      (diffa4Fixed map {_._1(10)}) -- (diffa5Fixed map {_._1(10)}) --
				      (diffa6Fixed map {_._1(10)}) -- (diffa7Fixed map {_._1(10)}) --
				      (diffa8Fixed map {_._1(10)}) -- (diffa9Fixed map {_._1(10)});
    val diffa10Fixed = matchesLOnly1.filter(p => (p._1(10) == a10));
    a11 <- candD(11).take(i).toSet -- (diffa0Fixed map {_._1(11)}) -- (diffa1Fixed map {_._1(11)}) -- 
   				      (diffa2Fixed map {_._1(11)}) -- (diffa3Fixed map {_._1(11)}) --
				      (diffa4Fixed map {_._1(11)}) -- (diffa5Fixed map {_._1(11)}) --
				      (diffa6Fixed map {_._1(11)}) -- (diffa7Fixed map {_._1(11)}) --
				      (diffa8Fixed map {_._1(11)}) -- (diffa9Fixed map {_._1(11)}) --
				      (diffa10Fixed map {_._1(11)});
    val diffa11Fixed = matchesLOnly1.filter(p => (p._1(11) == a11));
    a12 <- candD(12).take(i).toSet -- (diffa0Fixed map {_._1(12)}) -- (diffa1Fixed map {_._1(12)}) -- 
   				       (diffa2Fixed map {_._1(12)}) -- (diffa3Fixed map {_._1(12)}) --
				       (diffa4Fixed map {_._1(12)}) -- (diffa5Fixed map {_._1(12)}) --
				       (diffa6Fixed map {_._1(12)}) -- (diffa7Fixed map {_._1(12)}) --
				       (diffa8Fixed map {_._1(12)}) -- (diffa9Fixed map {_._1(12)}) --
				       (diffa10Fixed map {_._1(12)}) -- (diffa11Fixed map {_._1(12)});
    val diffa12Fixed = matchesLOnly1.filter(p => (p._1(12) == a12));
    a13 <- candD(13).take(i).toSet -- (diffa0Fixed map {_._1(13)}) -- (diffa1Fixed map {_._1(13)}) -- 
   				       (diffa2Fixed map {_._1(13)}) -- (diffa3Fixed map {_._1(13)}) --
				       (diffa4Fixed map {_._1(13)}) -- (diffa5Fixed map {_._1(13)}) --
				       (diffa6Fixed map {_._1(13)}) -- (diffa7Fixed map {_._1(13)}) --
				       (diffa8Fixed map {_._1(13)}) -- (diffa9Fixed map {_._1(13)}) --
				       (diffa10Fixed map {_._1(13)}) -- (diffa11Fixed map {_._1(13)}) --
				       (diffa12Fixed map {_._1(13)});
    val diffa13Fixed = matchesLOnly1.filter(p => (p._1(13) == a13));
    a14 <- candD(14).take(i).toSet -- (diffa0Fixed map {_._1(14)}) -- (diffa1Fixed map {_._1(14)}) -- 
   				       (diffa2Fixed map {_._1(14)}) -- (diffa3Fixed map {_._1(14)}) --
				       (diffa4Fixed map {_._1(14)}) -- (diffa5Fixed map {_._1(14)}) --
				       (diffa6Fixed map {_._1(14)}) -- (diffa7Fixed map {_._1(14)}) --
				       (diffa8Fixed map {_._1(14)}) -- (diffa9Fixed map {_._1(14)}) --
				       (diffa10Fixed map {_._1(14)}) -- (diffa11Fixed map {_._1(14)}) --
				       (diffa12Fixed map {_._1(14)}) -- (diffa13Fixed map {_._1(14)});
    val diffa14Fixed = matchesLOnly1.filter(p => (p._1(14) == a14));
    a15 <- candD(15).take(i).toSet -- (diffa0Fixed map {_._1(15)}) -- (diffa1Fixed map {_._1(15)}) -- 
   				       (diffa2Fixed map {_._1(15)}) -- (diffa3Fixed map {_._1(15)}) --
				       (diffa4Fixed map {_._1(15)}) -- (diffa5Fixed map {_._1(15)}) --
				       (diffa6Fixed map {_._1(15)}) -- (diffa7Fixed map {_._1(15)}) --
				       (diffa8Fixed map {_._1(15)}) -- (diffa9Fixed map {_._1(15)}) --
				       (diffa10Fixed map {_._1(15)}) -- (diffa11Fixed map {_._1(15)}) --
				       (diffa12Fixed map {_._1(15)}) -- (diffa13Fixed map {_._1(15)}) --
				       (diffa14Fixed map {_._1(15)});
    val x = List(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15)
    //if (matchesLOnly3.foldLeft(true)((r,c) => (r && (commonPos(x, c._1).length == c._2)) ))
) yield x

val end = System.currentTimeMillis

print("Took " + (end -start)/1000.0 + " s.")

res

}

import java.io.File
import java.io.FileInputStream
import java.nio.channels.FileChannel.MapMode._
import java.nio.ByteOrder._
import java.nio.{ByteBuffer,CharBuffer}

type Event = Int
val Event = Int

final class EventStream(buffer: ByteBuffer, position: Int) extends Stream[Event]
{
  private def readEvent: Event = {
    // read an event from the buffer starting at its current position
  }

  private lazy val (event, nextPosition) = {
    assert(!isEmpty)
    buffer.position(position)  // reset the buffer's position
    val e = readEvent
    (e, buffer.position)
  }

  override def head = event

  override def tail = {
    if (!tailDefined)
      throw new UnsupportedOperationException
    else
      new EventStream(buffer, nextPosition)
  }

  override def isEmpty = position == buffer.capacity

  protected def tailDefined = nextPosition <= buffer.capacity
}

def p187 = {

val file = new File("/media/ubuntuPart2/docs/topcoder/primes.txt")
val fileSize = file.length
val stream = new FileInputStream(file)
val buffer = stream.getChannel.map(READ_ONLY, 0, fileSize)

buffer.order(LITTLE_ENDIAN)

//val i = buffer.getInt  // reads a 32-bit little-endian integer

val events = new EventStream(buffer, 0)

val pr = events.take(4).toList

println("pr = " + pr) 
/*for (e <- events) {
    // process event
}*/

}

import scalax.io._
import scalax.file.{ FileOps, Path, NotFileException }

def p187 = {
val file = "/media/ubuntuPart2/docs/topcoder/primes.txt"
val ft:Input = Resource.fromFile(file)
val text = ft.slurpString(Codec.UTF8)

text

}

def p187 = { 
val file = new File("/media/ubuntuPart2/docs/topcoder/primes.txt")
val lines = fromFile( file ).getLines()
val rf = lines.filter(_!="")
val n = 0 

var cand = primes.takeWhile(_<50000)

rf foreach {
   pli => 
   primes = pli.trim.split(" ").filter(_!="").map(_.toInt)
   primes foreach {
     p => 
     n = n + cand.length
     cand = cand.tail
}
}

def p187 = { 
val file = new File("/media/ubuntuPart2/docs/topcoder/primes2.txt")
val lines = fromFile( file ).getLines()
lines
}

import scalax.io._
import scalax.file.{ FileOps, Path, NotFileException }

val primes3: Stream[Int] = {
    var last = 2
    2 #:: Stream.from(last+1).filter(candidate => {
      last=candidate
      primes3.takeWhile(_ < scala.math.sqrt(last).toInt+1).forall(candidate % _ != 0)
    })
  }

def p187(maxV: Int) = {
  //the biggest prime smaller than 10^4 is the 1228 prime
  val p = 9973
  val indexp = 1228
  val first1228Primes = primes3 take 1229
  /* for p < 9973 there are indexp + 1 composite nbs with exactly 2 divs (p*primes(indexp-1), p*primes(indexp-2), ..., p*2)
   * so in total \sum n(p) with p < 9973
   * n(2) = |{2*2}| = 1 //the nb of primes smaller or equal to 2 (or the nb of primes visited so far + 1)
     * n(3) = |{3*2, 3*3}| = 2 //the nb of primes smaller or equal to 3 (or the nb of primes visited so far + 1)
     * n(5) = |{5*2, 5*3, 5*5}| = 3 
     * n(7) = |{7*2, 7*3, 7*5, 7*7}| = 4 
     * ...
     * n(9973) = |{9973*2, 9973*3, ..., 9973*9967}| = 1229
     */ 
  val nbelow9973 = List.range(1, 1230).foldLeft(0)(_+_)

  var cand = first1228Primes.toList.reverse
  //the biggest prime smaller than 10^8/2 is 49979687
  //val pMax = 49979687

  val start = System.currentTimeMillis

  val file1 = "/media/ubuntuPart2/docs/topcoder/primes1.txt"
  val rf1: Input = Resource.fromFile(file1)

  val lines1 = rf1.lines().filter(_!="")
  val nlines1 = lines1.size
  println("n lines = " + nlines1)
  var n = 0L 
  var s = ""
  var clines = 0
  
  lines1 foreach {
    pli => 
      clines = clines + 1
      val cprimes = pli.trim.split(" ").filter(_!="").map(_.toInt)
      //if (clines >= (nlines - 1)) println("primes = " + cprimes.toList)
      cprimes foreach {
	p => 
	  cand = cand.dropWhile(c => (1L * c * p > maxV)) // || c > p))
	  if (clines == (nlines1 - 1)) println("p = " + p + " \n cand " + cand.toList)
	  n = n + cand.toList.length
	//s = s + "," + cand.toList.length
      }
  }

  println("after 1st round = " + n)

  val file2 = "/media/ubuntuPart2/docs/topcoder/primes2.txt"
  val rf2: Input = Resource.fromFile(file2)

  val lines2 = rf2.lines().filter(_!="")
  val nlines2 = lines2.size
  println("n lines2 = " + nlines2)
  
  clines = 0

  lines2 foreach {
   pli => 
   clines = clines + 1
   val cprimes = pli.trim.split(" ").filter(_!="").map(_.toInt)
   //if (clines >= (nlines - 1)) println("primes = " + cprimes.toList)
   cprimes foreach {
     p => 
     cand = cand.dropWhile(c => (1L * c * p > maxV))// || c > p))
     if (clines == (nlines2 - 1)) println("p = " + p + " \n cand " + cand.toList)
     n = n + cand.toList.length
     //s = s + "," + cand.toList.length
   }
  }

println("after 2nd round = " + n)

val file3 = "/media/ubuntuPart2/docs/topcoder/primes3.txt"
val rf3: Input = Resource.fromFile(file3)

val lines3 = rf3.lines().filter(_!="")
val nlines3 = lines3.size
println("n lines3 = " + nlines3)

clines = 0

lines3 foreach {
   pli => 
   clines = clines + 1
   val cprimes = pli.trim.split(" ").filter(_!="").map(_.toInt)
   //if (clines >= (nlines - 1)) println("primes = " + cprimes.toList)
   cprimes foreach {
     p => 
     cand = cand.dropWhile(c => (1L * c * p > maxV))// || c > p))
     if (clines == (nlines3 - 1)) println("p = " + p + " \n cand " + cand.toList)
     n = n + cand.toList.length
     //s = s + "," + cand.toList.length
   }
}

  println("after 3rd round = " + n)

  val end = System.currentTimeMillis

  print("Took " + (end -start)/1000.0 + " s.")

  nbelow9973 + n

}

def p187Aux(n: Int) = {
val maxV = math.pow(10, n).toInt
val primesLessMaxV = primes3.takeWhile(_ < maxV)

val sqrtMaxV = math.sqrt(maxV)
val parts = primesLessMaxV.partition(_ < sqrtMaxV)
//for the list of primes smaller than 10^(n/2) we know the nb of semiprimes is 1 +...+ parts._1.length
println("the biggest prime smaller than 10^(n/2) = " + parts._1.last)
val l = parts._1.toList.length
val nsp1 = List.range(1, l+1).foldLeft(0)(_+_)
var nsp2 = nsp1
var cand = parts._1.reverse
parts._2 foreach {
  p => 
    cand = cand.dropWhile(c => c * p > maxV)
    nsp2 = nsp2 + cand.length  
}
nsp2
}

def p187(maxV: Int) = {
  //the biggest prime smaller than 10^4 is the 1228 prime
  val p = 9973
  val indexp = 1228
  val first1228Primes = primes3 take 1229
  /* for p < 9973 there are indexp + 1 composite nbs with exactly 2 divs (p*primes(indexp-1), p*primes(indexp-2), ..., p*2)
   * so in total \sum n(p) with p < 9973
   * n(2) = |{2*2}| = 1 //the nb of primes smaller or equal to 2 (or the nb of primes visited so far + 1)
     * n(3) = |{3*2, 3*3}| = 2 //the nb of primes smaller or equal to 3 (or the nb of primes visited so far + 1)
     * n(5) = |{5*2, 5*3, 5*5}| = 3 
     * n(7) = |{7*2, 7*3, 7*5, 7*7}| = 4 
     * ...
     * n(9973) = |{9973*2, 9973*3, ..., 9973*9967}| = 1229
     */ 
  val nbelow9973 = List.range(1, 1230).foldLeft(0)(_+_)

  var cand = first1228Primes.toList.reverse
  //the biggest prime smaller than 10^8/2 is 49979687
  //val pMax = 49979687

  val start = System.currentTimeMillis

  val file1 = "/media/ubuntuPart2/docs/topcoder/allprimes.txt"
  val rf1: Input = Resource.fromFile(file1)

  val lines1 = rf1.lines().filter(_!="")
  val nlines1 = lines1.size
  println("n lines = " + nlines1)
  var n = 0L 
  var s = ""
  var clines = 0
  
  lines1 foreach {
    pli => 
      clines = clines + 1
      val cprimes = pli.trim.split(" ").filter(_!="").map(_.toInt)
      //if (clines >= (nlines - 1)) println("primes = " + cprimes.toList)
      cprimes foreach {
	p => 
	  cand = cand.dropWhile(c => (1L * c * p > maxV)) // || c > p))
	  if (clines == (nlines1 - 1)) println("p = " + p + " \n cand " + cand.toList)
	  n = n + cand.toList.length
	//s = s + "," + cand.toList.length
      }
  }
  n + nbelow9973
}

def p179J(limit: Int) = {
val time = System.currentTimeMillis
val ndiv = Array.fill(limit)(1)
ndiv(0) = 0 
var p = 2
while ( p < limit ) {
if ( ndiv(p) == 1 ) 
    {
	var j = 1
	while ( j*p < limit ) {
	  var count = 2
          var t = j
          while ( t%p == 0 ) {
	    count = count + 1
            t = t / p
          }
          ndiv(j*p) = ndiv(j*p)*count
	  j = j + 1
        }
    }
p = p + 1
}

var result = 0
var i = 0
while ( i < ndiv.length - 1) {
  if ( ndiv(i) == ndiv(i+1) ) result = result + 1
  i = i + 1
}
println( "Result: " + result)
println( "Time (s): " + ( System.currentTimeMillis() - time ) / 1000. )
}

def p179(n: Int) = {
val time = System.currentTimeMillis
val limit = math.pow(10, n).toInt
val divs = new Array[Int](limit)
var i = 2

while (i < limit) {
  var j = 2
  var sqrti = math.sqrt(i).toInt
  while (j <= sqrti) {
    if (i % j == 0) if (i == j * j) divs(i) = divs(i) + 1 else divs(i) = divs(i) + 2 
    j = j + 1 }
  i = i + 1
}

i = 2
var result = 0 
while ( i < limit - 1) {
  if (divs(i) == divs(i + 1)) result = result + 1
  //if (i < 50) println("divs(" + i + ") = " + divs(i))
  i = i + 1
  }

println( "Result: " + result)// + " sieb = " + sieb.toList)
println( "Time (s): " + ( System.currentTimeMillis() - time ) / 1000. )
}

def p179F(n: Int) = {
val time = System.currentTimeMillis
val limit = math.pow(10, n).toInt
val divs = new Array[Int](limit)

var i = 2
while (i < limit) {
  var j = i
  while (j < limit) {
    divs(j) = divs(j) + 1
    j = j + i }
  i = i + 1
}

i = 2
var result = 0 
while ( i < limit - 1) {
  if (divs(i) == divs(i + 1)) result = result + 1
  //if (i < 50) println("divs(" + i + ") = " + divs(i))
  i = i + 1
  }

println( "Result: " + result)// + " sieb = " + sieb.toList)
println( "Time (s): " + ( System.currentTimeMillis() - time ) / 1000. )
}

def p179F2(n: Int) = {
val time = System.currentTimeMillis
val limit = math.pow(10, n).toInt
val sqrtL = math.sqrt(limit)
val divs = new Array[Int](limit)

var i = 2
while (i <  sqrtL) {
  var j = i*i
  divs(j) = divs(j) + 1
  j = j + i
  while (j < limit) {
    divs(j) = divs(j) + 2
    j = j + i
  }
  i = i + 1
}

i = 2
var result = 0 
while ( i < limit - 1) {
  if (divs(i) == divs(i + 1)) result = result + 1
  //if (i < 50) println("divs(" + i + ") = " + divs(i))
  i = i + 1
}

println( "Result: " + result)
println( "Time (s): " + ( System.currentTimeMillis() - time ) / 1000. )
}

p179F2(7)

def tri(i: Int) = if (i%2 == 0) (i/2)*(i + 1) else i*((i + 1)/2)

def p12J(limit: Int) = {
val time = System.currentTimeMillis
val sieb = Array.fill(limit)(1)
sieb(0) = 0 
var p = 2
while ( p < limit ) {
if ( sieb(p) == 1 ) 
    {
	var j = 1
	while ( j*p < limit ) {
	  var count = 2
          var t = j
          while ( t%p == 0 ) {
	    count = count + 1
            t = t / p
          }
          sieb(j*p) = sieb(j*p)*count
	  j = j + 1
        }
    }
p = p + 1
}

var i = 0
while ( (i < limit - 1) && ((i % 2 == 0) && (sieb(i/2)*sieb(i+1) < 500) || (sieb(i)*sieb((i+1)/2) < 500)) ) {
  i = i + 1 
}

println( "Result: i = " + i + " tri(i) = " + tri(i))
println( "Time (s): " + ( System.currentTimeMillis() - time ) / 1000. )
}

p12J(10000000)

def p145J(k: Int) = {
val time = System.currentTimeMillis
val limit = math.pow(10, k).toInt
var n = 12
var result = 0
val EvenSet = Set('0', '2', '4', '6', '8')
while ( n < limit ) {
  if (n % 10 != 0) { 
    val nr = n.toString.reverse.toInt
    if (( (n + nr).toString.toSet intersect EvenSet) == Set()) result = result + 1
  }
  n = n + 1
}

println( "Result: " + result) 
println( "Time (s): " + ( System.currentTimeMillis() - time ) / 1000. )
}

def getMiddle(k: Int) : List[(String, String)] = {
 if (k == 2) for (i <- List.range(0,10); j <- List.range(0,10)) yield (("" + i + j), ("" + j + i))
 else {
 val middlePrev = getMiddle(k-1)
 for(c <- middlePrev; j <- List.range(0,10)) yield (("" + c._1 + j), ("" + j + c._2))
 }
}

def p145(k: Int) = {
val time = System.currentTimeMillis
val limit = math.pow(10, k).toInt

var result = 120
val middleL = getMiddle(k-2)
for (d0 <- List(1,3,5,7); middle <- middleL; dk <- List(2,4,6,8).filter(_>d0); if allOdd((d0 + middle._1).toInt + (middle._2 + dk))) 
println( "Result: " + result) 
println( "Time (s): " + ( System.currentTimeMillis() - time ) / 1000. )
}


def p() = {
val time = System.currentTimeMillis
val limit = math.pow(10, k).toInt

println( "Result: " + result) 
println( "Time (s): " + ( System.currentTimeMillis() - time ) / 1000. )
}

import scalax.io._
import scalax.file.{ FileOps, Path, NotFileException }

def p139(k: Int) = {
  val start = System.currentTimeMillis
  val limit = math.pow(10,k).toInt
  val file = "/media/ubuntuPart2/docs/topcoder/pythagorean-triples.txt"
  val rf: Input = Resource.fromFile(file)

  val lines = rf.lines().filter(_!="")
  val nlines = lines.size
  println("n lines = " + nlines)
  var n = 0
  var clines = 0
  var goodTrips = List[List[Int]]()

  def valid(triple: Array[Int]) = (triple(2) % (math.abs(triple(1)-triple(0))) == 0)

  lines foreach {
    li => 
      clines = clines + 1
      val tmp = li.trim.split(" ").filter(_!="") map {_.toInt}
      if (tmp.length >= 3) {
	val ctriple = tmp.take(3)  
	val striple = ctriple.foldLeft(0)(_+_)
	if (striple <= limit && valid(ctriple)) {
	  /* if (a,b,c) is a sol then so is (ka, kb, kc) 
	   * so we need to compute how many k exist s.t.
	   * ka + kb + kc <= 10^9, i.e.,
	   * k <= 10^9/(a+b+c)
	   */ 
	val naux = (limit / striple)
	//if (clines < 500 || clines >= (nlines - 1)) println("last trip = " + ctriple.toList + " gives naux = " + naux)
	n = n + naux
	//goodTrips = ctriple.toList+:goodTrips
	}
      }
  }
  println( "Result: " + n)//goodTrips) 
  println( "Time (s): " + ( System.currentTimeMillis() - start ) / 1000. )
  n 
}

p139(9)


lazy val gstream: Stream[BigInt] = Stream.cons(1, Stream.cons(4, gstream.zip(gstream.tail).map(gi => gi._1 + gi._2)))

def check140(a: BigInt, b: BigInt, j: Int, diff: Double) = {
  var i = 0
  var ai = a
  var bi = b
  var s = a*gstream(0)
  while ( (((j*bi.toDouble - s.toDouble)/bi.toDouble) > diff)  && i < 2500) {
    if (i < 15) println("i = " + i + " s = " + s + 
			" gi = " + gstream(i) + "bi*j" + (bi*j) + 
			" diff = " + (bi*j - s) + " diff/bi = " + ((bi*j - s)/bi))
    i = i + 1
    bi = bi * b
    ai = ai * a
    s = s*b + ai*gstream(i)
  }
  println("i = " + i + " s = " + s + " bi*j = " + bi*j)
}

def f(x: Int) = { 
  val aux = 5*x*x - 16*x + 4
  val sqrtAux = math.sqrt(aux).toInt
  (sqrtAux*sqrtAux == aux, sqrtAux)
}

lazy val kStream: Stream[Int] = Stream.from(2).filter(x => f(x)._1)

def p140(lim: Int) = {
  var i = 0 
  var ngn = 0
  while(ngn < lim && i < 1000) {
    val k = kStream(i) 
    val delta = f(k)
    val aCand = List.range(1, delta._2 / (k-4))
    val bCand = aCand map {a => (a, (k-2)*a + delta._2)}  
    val ab = (bCand.filter{x => (x._2 % (2*(k-3))) == 0}) map {x => (x._1, x._2 / (2*(k-3)))}
    i = i + 1  
    if (ab != List()) println("k = " + k + " delta = " + delta + " ab = " + ab)
  }
}

def p140(limi: Int, lim: Int) = {
var a = 1
var b = 2
var i = 0
var ngn = 0
var visited = Set[BigInt]()
while (i < limi) {
  while(a < lim) {
    b = (3*a)/2 + 1 
    while(b < lim) {
      val k1 = (3L*a+1L*b)*a
      val k2 = 1L*b*(b-a) - 1L*a*a
      if ((k2 > 0) && ((k1 % k2) == 0) && (!visited(k1/k2))) {
	visited = visited + (k1/k2)
	println("a = " + a + " b = " + b + " k1 = " + k1 + " k2 = " + k2 + " k1/k2 = " + (k1/k2))
	ngn = ngn + 1
      }
   b = b + 1
   }
  a = a + 1
  }
 i = i + 1  
}
println("n = " + ngn)
visited
}



def f(lim: Int) = {
var a = 1
var s = 0L
var res = 0
var gn = List[Long]()
while(a < lim && res < 30) {
  val delta21 = 5L*a*a + 4L
  val deltaR1 = isPerfectSquare(delta21)
  if (deltaR1._1) {
    val b = (a + deltaR1._2)/2
    val n = (3L*a+1L*b)*a
    println("1: a = " + a + " b = " + b + " found " + n)
    s = s + n
    gn = n +: gn
    res = res + 1
  }
  val delta211 = 5L*a*a + 44L
  val deltaR11 = isPerfectSquare(delta211)
  if (deltaR11._1) {
    val b = (a + deltaR11._2)/2
    val n = (3L*a+1L*b)*a
    if (n % 11 == 0) {
    println("11: a = " + a + " b = " + b + " found " + n/11)
    s = s + n/11
    res = res + 1 
    gn = (n/11) +: gn
    }
  }
  a = a + 1
}
(s, res, gn)
}

def isPerfectSquare(n: BigInt) = {
  val nsqrt = math.sqrt(n.toDouble).toLong
  (1L*nsqrt*nsqrt == n, nsqrt)
}

def isPerfectSquare(n: Long) = {
  val nsqrt = math.sqrt(n.toDouble)
  val nsqrtL = nsqrt.toLong
  (nsqrt == nsqrtL, nsqrtL)
}

def p140Slow(lim: BigInt) = {
var n = 2L
var s = 0L
var result = 0
while(n < lim && result < 30) {
  if (Set(0,1,2,5,6,7)((n % 10).toInt)) {
    val cand = 5L*n*n + 14L*n + 1L
    if(isPerfectSquare(cand)) {
      println("found " + n)
      result = result + 1
      s = s + n
    }
  }
n = n + 1
}
s
}

def p137(limi: Int, lim: Int) = {
var a = 1
var b = 2
var i = 0
var ngn = 0
var visited = Set[Long]()
while (ngn < limi) {
  while(a < lim) {
    b = (3*a)/2 + 1 
    while(b < lim) {
      val k1 = 1L*b*a
      val k2 = 1L*b*(b-a) - 1L*a*a
      if ((k2 > 0) && ((k1 % k2) == 0) && (!visited(k1/k2))) {
	visited = visited + (k1/k2)
	println("a = " + a + " b = " + b + " k1 = " + k1 + " k2 = " + k2 + " k1/k2 = " + (k1/k2))
	ngn = ngn + 1
      }
   b = b + 1
   }
  a = a + 1
  }
 i = i + 1  
}
println("after " + i + " iters n = " + ngn)
(visited, visited.foldLeft(0L)(_+_))
}

def p137Fin(lim: Int) = {
var a = 1
var s = 0L
var res = 0
var gn = List[Long]()
while(a < lim && res < 15) {
  val delta21 = 5L*a*a + 4L
  val deltaR1 = isPerfectSquare(delta21)
  if (deltaR1._1) {
    val b = (a + deltaR1._2)/2
    val n = 1L*b*a
    println("a = " + a + " b = " + b + " found " + n)
    s = s + n
    gn = n +: gn
    res = res + 1
  }
  a = a + 1
}
(s, res, gn.sorted)
}


def p138 = {
  val start = System.currentTimeMillis
  val file = "/media/ubuntuPart2/docs/topcoder/pythagorean-triples.txt"
  val rf: Input = Resource.fromFile(file)

  val lines = rf.lines().filter(_!="").toStream
  val nlines = lines.size
  println("n lines = " + nlines)
  var n = 0
  var s = 0
  var clines = 0
  var goodTrips = List[List[Int]]()

  def valid(triple: Array[Int]) = (math.abs(triple(0) - 2*triple(1)) == 1) || (math.abs(triple(1) - 2*triple(0)) == 1)

def f(triple: Array[Int]) = (math.abs(triple(0) - 2*triple(1)), math.abs(triple(1) - 2*triple(0)))

  val res = lines.dropWhile {
    li => 
      clines = clines + 1
      val tmp = li.trim.split(" ").filter(_!="") map {_.toInt}
      if (tmp.length >= 3) {
	val ctriple = tmp.take(3)  
	if ((clines < 20) || (clines > nlines - 5)) println("ctriple = " + ctriple.toList + " diff = " + f(ctriple))
	if (valid(ctriple)) {
	n = n + 1
	s = s + ctriple(2)  
	goodTrips = ctriple.toList+:goodTrips
	}	
      }
    (n < 12)
  }

  println( "Result: " + n + " goodtrips = " + goodTrips + " s = " + s) 
  println( "Time (s): " + ( System.currentTimeMillis() - start ) / 1000. )
  res
}

p138

LongTraversable(1,3,4,2,5,6).toStream.dropWhile {
 i => 
   println("i = " + i)
   if (i%2 == 0) n = n + 1
   (n < 1)
}

def p138P2 = {
  val start = System.currentTimeMillis
  val file = "/media/ubuntuPart2/docs/topcoder/pythagorean-LONG.txt"
  val rf: Input = Resource.fromFile(file)

  val lines = rf.lines().filter(_!="").toStream
  val nlines = lines.size
  println("n lines = " + nlines)
  var n = 0
  var s = 0L
  var clines = 0
  var goodTrips = List[List[Long]]()

  def valid(triple: Array[Long]) = (math.abs(triple(0) - 2L*triple(1)) == 1) || (math.abs(triple(1) - 2*triple(0)) == 1)

  def f(triple: Array[Long]) = (math.abs(triple(0) - 2*triple(1)), math.abs(triple(1) - 2*triple(0)))

  val res = lines.dropWhile {
    li => 
      clines = clines + 1
      if (clines > nlines - 2) println("li = " + li.toList)
      val tmp = li.trim.split(" ").filter(_!="") map {_.toLong}
      if (tmp.length >= 3) {
	val ctriple = tmp.take(3)  
	if (clines > nlines - 2) println("ctriple = " + ctriple.toList + " diff = " + f(ctriple))
	if (valid(ctriple)) {
	n = n + 1
	s = s + ctriple(2)  
	goodTrips = ctriple.toList+:goodTrips
	}	
      }
    (n < 9)
  }

  println( "Result: " + n + " goodtrips = " + goodTrips + " s = " + s) 
  println( "Time (s): " + ( System.currentTimeMillis() - start ) / 1000. )
  res
}

p138P2

def isTriangle(a: Int, b: Int, c: Int) = (a < b + c) && (b < c + a) && (c < a + b)

def p143(lim: Int) = {
val start = System.currentTimeMillis
var s = 1
var a = 1
var b = 2 
var c = 3
var res = 0
var found = false
while (s <= lim) {
  //if (s < 10) println("s = " + s + " (a,b,c) = " + (a,b,c))
  found = false
  c = s - 1  
  while (c >= 1 && !found) {
    b = c - math.sqrt(s).toInt
    while (b >= 1 && !found) {
      //at this point i can already check if s / (c2 - b2) 
      val b2 = b*b
      val c2 = c*c
      if ( (c2 - b2) % s == 0 ) {
	val s2 = s*s
	val al = math.max((c - b), math.sqrt(s2 - b2 - c2).toInt)
	a = al
	//(a,b,c) must form a triangle
	while(a < b && !found) {
	    //if (c < 20) println("triangle = " + (a,b,c))
	    val a2 = a*a
	    if ( ( (b2 - a2) % s == 0 ) && ( (c2 - a2) % s == 0 ) ) {
	      res = res + s
	      found = true
	      if (s < 20 || s > lim - 2) 
	        println("found (a,b,c) = " + (a,b,c) + " at s = " + s)	    
	    }
	  a = a + 1
	}
      }
      b = b - 1
    }
    c = c - 1
  }
s = s + 1
}
println( "Time (s): " + ( System.currentTimeMillis() - start ) / 1000. )
res
}

p143(1000)


def p143Slow(lim: Int) = {
  val start = System.currentTimeMillis
  var s = 1
  var a = 1
  var b = 2 
  var c = 3
  var res = 0
  var sL = List.range(4,120001).toSet
  while (c <= lim) {
    b = c - 1
    while (b >= 1) {
      //at this point i can already check if s / (c2 - b2) 
      val b2 = b*b
      val c2 = c*c
      if ( (c2 - b2) % s == 0 ) {
	a = b - 1
	//(a,b,c) must form a triangle
	while(a >= (c - b)) {
	  if (isTriangle(a, b, c)) {
	    //if (c < 20) println("triangle = " + (a,b,c))
	    val a2 = a*a
	    sL foreach {
	      s => 
		if ( ( a2 + b2 + c2 > s*s ) && 
		    ( (b2 - a2) % s == 0 ) && 	
		    ( (c2 - a2) % s == 0 ) ) {
		      sL = sL - s
		      res = res + s
		      if (s > lim - 2) println("found a = " + a + " b = " + b + " c = " + c + " s = " + s)
		    }
	    }
	  }
	  a = a - 1
	}
      }
      b = b - 1
    }
    c = c + 1
  }
println( "Time (s): " + ( System.currentTimeMillis() - start ) / 1000. )
res

}

def p143T1(lim: Int) = {
val start = System.currentTimeMillis
val res = for(p <- List.range(1,lim+1); q <- List.range(p, lim + 1); val r = isPerfectSquare(1L*p*p + 1L*q*q + 1L*p*q); if (r._1)) yield (p, q, r._2)
println( "Time (s): " + ( System.currentTimeMillis() - start ) / 1000. )
res
}

p143T1(1000)

def p143T2(lim: Int) = {
val start = System.currentTimeMillis
var p = 1
var q = 2
var res = List[(Int,Int,Any)]()
while (p <= lim) {
  q = p + 1
  while (q <= lim) {
    val r = isPerfectSquare(1L*p*p + 1L*q*q + 1L*p*q)
    if (r._1) res = (p,q,r._2) +: res
    q = q + 1
  }
  p = p + 1
}
println( "Time (s): " + ( System.currentTimeMillis() - start ) / 1000. )
res
}

p143T2(1000)

/** OBS: for lim = 1000 p143T1 (with range) takes 0.4 while p143T2 (without range, only while loops) takes 0.02
*/

def p143(lim: Int) = {
  val start = System.currentTimeMillis
  var p = 1
  var q = 2
  var res = List[(Int,Int,Any)]()
  while (p <= lim) {
    q = p + 1
    while (q < lim - p) {
      val aux = isPerfectSquare(1L*p*p + 1L*q*q + 1L*p*q)
      if (aux._1) res = (p,q,aux._2) +: res
      q = q + 1
    }
    p = p + 1
  }

  println("nb of pq pairs = " + res.length)
  var sum = 0
  var nsol = 0
  var visited = Set[Int]()
  res foreach {
    pq =>
      val p = pq._1
      val q = pq._2
      var spq = p + q
      var r = q + 1
      while (r <= lim - spq) {
	val aux = isPerfectSquare(1L*p*p + 1L*r*r + 1L*p*r)
	if (aux._1) {
  	  val aux1 = isPerfectSquare(1L*q*q + 1L*r*r + 1L*q*r)
	  val spqr = spq + r
	  if (aux1._1 && !visited(spqr)) {
		sum = sum + spqr
		visited = visited + spqr
		nsol = nsol + 1
		if (nsol < 8 || nsol > 500) // (spq + r) > (lim - 10) )
		  println("found (p,q,r) = " + (p,q,r) + " s = " + (spqr) + " (a,b,c) = " + (pq._3, aux._2, aux1._2))
	  }
	}
	r = r + 1
      }
  }

  println( "Time (s): " + ( System.currentTimeMillis() - start ) / 1000. )
  (sum, nsol)
}

p143(1000)

/** compared to the above impl this is 3.5 faster: the diff is that instead of searching for r from scratch we make ops on maps. */ 

def p143Faster(lim: Int) = {
  val start = System.currentTimeMillis
  var p = 1
  var q = 2
  var res = List[(Int,Int,Any)]()
  var pqMap = Map[Int, Set[Int]]().empty
  while (p <= lim) {
    q = p + 1
    while (q < lim - p) {
      val aux = isPerfectSquare(1L*p*p + 1L*q*q + 1L*p*q)
      if (aux._1) pqMap = if (pqMap.keys.toSet contains p) pqMap.updated(p, pqMap(p)+q) else pqMap.updated(p, Set(q))
	//res = (p,q,aux._2) +: res
      q = q + 1
    }
    p = p + 1
  }

  println("nb of pairs = " + pqMap.foldLeft(0)(_+_._2.size))
  pqMap = pqMap.filter(_._2.size > 1)  
  val pRange = pqMap.keys.toSet

  var sum = 0
  var nsol = 0
  var visited = Set[Int]()
  pqMap foreach {
    pEntry =>
      val p = pEntry._1
      val qRange = pEntry._2 intersect pRange
      qRange foreach {
	q => 
	  val rRange = pqMap(q) intersect pEntry._2
	  rRange foreach {
	    r => 
	      val s = p + q + r
	      if (s <= lim && !visited(s)) {
		visited = visited + s
		//if (nsol 10) println("found (p,q,r) = " + (p,q,r) + " pqMap(p) = " + pqMap(p) + " pqMap(q) = " + pqMap(q) )
		sum = sum + s
		nsol = nsol + 1
	    }
	  }
      }      
  }

  println( "Time (s): " + ( System.currentTimeMillis() - start ) / 1000. )
  (sum, nsol)
}

p143(1000)

import scalax.io._
import scalax.file.{ FileOps, Path, NotFileException }

def getN134(p1: Int, p2: Int) = {
  var l = p1.toString.length
  var base = math.pow(10, l).toLong
  var k = 1
  var n = base*k + p1
  while (n % p2 != 0) {
    k = k + 1
    n = 1L * base * k + p1
  }
  n
}

def p134 = {
  val start = System.currentTimeMillis
  val file = "/media/ubuntuPart2/docs/topcoder/primesSmallerThan10to6.txt"
  val rf: Input = Resource.fromFile(file)

  val lines = rf.lines().filter(_!="")
  val nlines = lines.size
  println("n lines = " + nlines)
  var sum = 0L 
  var prev = 5
  var clines = 0
  
  lines foreach {
    pli => 
      clines = clines + 1
      val cprimes = pli.trim.split(" ").filter(_!="").map(_.toInt).toList
      //if (clines >= (nlines - 1)) println("primes = " + cprimes.toList)
      val nV = getN134(prev, cprimes(0))
      if (clines > 6560 && clines < 6570) println("for (p1, p2) = " + (prev, cprimes(0)) + " found nV = " + nV)
      sum = sum + nV
      prev = cprimes.last
      val n = cprimes.length
      sum = List.range(0, n - 1).foldLeft(sum)((r,c)=>(r + getN134(cprimes(c), cprimes(c+1))))
  }

  println( "Time (s): " + ( System.currentTimeMillis() - start ) / 1000. )
  sum
}

p134

def p149 = {
  val s = new Array[Int](4000001)
  List.range(1,56) foreach {
    k => 
      s(k) = ((100003L - 200003L*k + 300007L*k*k*k) % 1000000).toInt - 500000
  }
  var k = 56
  while (k <= 4000000) {
    s(k) = ( (s(k-24) + s(k-55) + 1000000) % 1000000) - 500000
    k = k + 1
  }

  val sumM = new Array
}


def p368(lim: Int) = {
  val start = System.currentTimeMillis
  val eps = 1./math.pow(10,lim)
  val maxL = scala.Long.MaxValue
  var sold = 0.
  var s = 1.
  var i = 2L
  while ((s - sold) > eps && i < maxL) {
    sold = s
    while (i.toString.contains('9')) i = i + 1
    s = s + 1./i
    if (i > 8888888888888888880L) println("i = " + i + " s = " + s)
    i = i + 1
  }
  println( "Time (s): " + ( System.currentTimeMillis() - start ) / 1000. )
  (i, s)
}

p368(8)

def p368O(lim: Int) = {
  val start = System.currentTimeMillis
  val eps = 1./math.pow(10,lim)
  val maxL = scala.Long.MaxValue
  var sold = 0.
  var s = 1.
  var i = 2L
  while ((s - sold) > eps && i < maxL) {
    sold = s
    var is = i.toString
    while (is.contains('9') && i < maxL) {
      val p = is.indexOf('9')
      if (p > 0)
	i = (is.take(p-1) + (is(p-1) - 47)).toLong * math.pow(10, is.length-p).toLong
      else i = math.pow(10, is.length).toLong
      is = i.toString
    }
    s = s + 1./i
    if (i > 8888888888888888880L) println("i = " + i + " s = " + s)
    i = i + 1
  }
  println( "Time (s): " + ( System.currentTimeMillis() - start ) / 1000. )
  (i, s)
}

p368O(8)

def p368O(lim: Int) = {
  val start = System.currentTimeMillis
  val eps = 1./math.pow(10,lim)
  val maxL = math.pow(10,lim)//scala.Long.MaxValue
  var sold = 0.
  var s = 1.
  var i = 2L
  var si = Set(1L)
  while ((s - sold) > eps && i < maxL) {
    sold = s
    var p = 0
    while ((i % 10) == 9 && i < maxL) {      
      i = i / 10 + 1
      p = p + 1
    }
    if (p!=0) i = i * math.pow(10,p).toLong
    //println("i = " + i)
    si = si + i
    s = s + 1./i
    if (i > 88885 && i < 88910) println("i = " + i + " s = " + s)
    i = i + 1
  }
  println( "Time (s): " + ( System.currentTimeMillis() - start ) / 1000. )
  (si, s)
}

val res = p368O(8)


  def nextValid368(i: Long) = {
    var p = 0
    var lD1 = i % 10

    if (lD1 == 9) {

      var q1 = i / 10
      var lD2 = q1 % 10
      if ((lD1 + 1) == lD2) {
	var q2 = q2 / 10
	var lD3 = q2 % 10
	// if by adding one the last 3 digits become equal and the last digit is < 8 then i+2 will be valid 
	if (lD2 == lD3) i = i + 2
	else i = i + 1
      }
    }
    while ( (lD1 + 1) == lD2 && lD2 == lD3 && i < maxL ) {
      i = i / 10 + 1 
      p = p + 1
    }
    i = i * math.pow(10,p).toLong

  }

def p368O2(lim: Int) = {
  val start = System.currentTimeMillis
  val lim10 = math.pow(10,lim).toLong
  val eps = 1./lim10
  val maxL = scala.Long.MaxValue
  var sold = 0.
  var s = List.range(1,100).foldLeft(0.)(_+1./_)
  var i = 99L  
  var d = 1
  while (d <= 9) {    
    while (i < 111*d) {
      //if (i < 115) println("i = " + i)
      s = s + 1./i
      compute(i)
      i = i + 1
    }
    i = i + 1
    d = d + 1
  }  
  def compute(i: Long) : Unit = {
    if ( i < lim10 ) {
      if (i > 33219 && i < 33235) println(" i = " + i + " s " + s) 
      val lD1 = i % 10
      val q = i / 10
      val lD2 = q % 10
      if (lD1 == lD2) {
	var d = 0L
	while (d < lD1) {
	  val ni = i*10 + d
	  s = s + 1./ni
	  compute(ni)
	  d = d + 1
	}
	d = lD1 + 1
	while (d < 10) {
	  val ni = i*10 + d
	  s = s + 1./ni
	  compute(ni)
	  d = d + 1
	}
      }
      else {
	var d = 0 
	while (d < 10) {
	  val ni = i*10 + d
	  s = s + 1./ni
	  compute(ni)
	  d = d + 1 
	}     
      }
    }
  }
  println( "Time (s): " + ( System.currentTimeMillis() - start ) / 1000. )
  (i, s)
}  

p368O2(10)

def p368O3(lim: Int) = {
  val start = System.currentTimeMillis
  val lim10 = math.pow(10,lim).toLong
  val eps = 1./lim10
  val maxL = scala.Long.MaxValue
  var sold = 0.
  var s = List.range(1,101).foldLeft(0.)(_+1./_)
  var m = (List.range(3,18) map { x => x -> Set(math.pow(10,x).toLong)}).toMap
  var i = 101L  
  var d = 1
  while (d <= 9) {    
    while (i < 111*d) {
      //if (i < 115) println("i = " + i)
      s = s + 1./i
      m = m.updated(3, m(3) + i)
      compute(i,3)
      i = i + 1
    }
    i = i + 1
    d = d + 1
  }  
  def compute(i: Long, li: Int) : Unit = {
      val lD1 = i % 10
      val q = i / 10
      val lD2 = q % 10
      if (lD1 == lD2) {
	var d = 0L
	while (d < lD1) {
	  val ni = i*10 + d
	  s = s + 1./ni
	  m = m.updated(li+1, m(li+1) + ni)
	  d = d + 1
	}
	d = lD1 + 1
	while (d < 10) {
	  val ni = i*10 + d
	  s = s + 1./ni
	  m = m.updated(li+1, m(li+1) + ni)
	  d = d + 1
	}
      }
      else {
	var d = 0 
	while (d < 10) {
	  val ni = i*10 + d
	  s = s + 1./ni
	  m = m.updated(li+1, m(li+1) + ni)
	  d = d + 1 
	}     
      }
  }
  List.range(4,lim) foreach {
    j => 
      m(j) foreach { x => compute(x, j) }
  }
  println( "Time (s): " + ( System.currentTimeMillis() - start ) / 1000. )
  (s, m(5).size, m(6).toList.take(3))
}  

p368O3(10)

// computes \sum_{x\in s} 1/x^j
def harmonicP(s: Set[Long], j: Int) = s.foldLeft(0.)(_+1./math.pow(_, j))

def binomial(n: Int, j: Int) : Long = {
  if (j == 0 || n == j) 1
  else if (j == 1) n
	else binomial(n-1, j-1) + binomial(n-1, j)
}

/* computes           (n+j-1)!    0^n + 1^n + ... + 8^n
 *          (-1)^n * ---------- * --------------------- 
 *                    n!*(j-1)!            10^n
 */ 
def coeffFor9(j: Int, n: Int) = { 
  val r = binomial(j+n-1,n) * List.range(0, 9).foldLeft(0.)(_+math.pow(_,n)) / math.pow(10,n + j)
  if (n % 2 == 0) r else (0. - r)
}

def p368For9(limNDigits: Int, limPower: Int, limEps: Int, mk: Int) = {
  val start = System.currentTimeMillis
  val lim10 = math.pow(10,limEps).toLong
  val eps = 1./lim10
  val maxL = scala.Long.MaxValue
  val t: Array[Array[Double]] = new Array(limNDigits+1,limPower+1)
  var s = (List.range(1, 10) map {k => (k -> Set(math.pow(10, k-1).toLong))}).toMap
  s = s.updated(1, s(1) ++ Set(2L,3L,4L,5L,6L,7L,8L))
  var j = 1
  while (j <= limPower) {
    t(1)(j) = harmonicP(s(1), j)
    j = j + 1
  }
  println("t(1) : " + t(1).toList)
  List.range(2,mk) foreach {
    key => 
    s = s.updated(key, s(key-1).map{x => (s(1)+0L).map{d => (x*10 + d)}}.flatten)
    j = 1
    while (j <= limPower) {
      t(key)(j) = harmonicP(s(key), j)
      j = j + 1
    }
  println("t(" + key + ") : " + t(key).toList)
  }
  var i = mk  	     
  var found = false
  while (i <= limNDigits && !found) {
    t(i)(1) = List.range(0,limPower).foldLeft(0.)( (r, c) => (r + coeffFor9(1,c)*t(i - 1)(1 + c)))
    if (math.abs(t(i)(1)) < eps) found = true
    if (!found) {
      j = 2
      while (j <= limPower) {
	t(i)(j) = List.range(0,limPower-j+1).foldLeft(0.)( (r, c) => (r + coeffFor9(j,c)*t(i - 1)(j + c)))
	j = j + 1
      }
    }
    if (i < 10 || i > limNDigits - 2) println("t(" + i + ") : " + t(i).toList)
    i = i + 1
  }
  println( "Time (s): " + ( System.currentTimeMillis() - start ) / 1000. )
  List.range(1,i).foldLeft(0.)(_ + t(_)(1))
}  

p368For9(4,3,10,3)

def coeffForNonConsec(j: Int, n: Int) = { 
  val r = binomial(j+n-1,n) / math.pow(10,n + j) 
  if (n % 2 == 0) r else (0. - r)
}

def powerDigits(bound: List[Int], n: Int) = bound.foldLeft(0.)(_+math.pow(_,n)) 

def p368ForNonConsec(limNDigits: Int, limPower: Int, limEps: Int, mk: Int, bound: List[Int]) = {
  val start = System.currentTimeMillis
  val lim10 = math.pow(10,limEps).toLong
  val eps = 1./lim10
  val maxL = scala.Long.MaxValue
  val t: Array[Array[Double]] = new Array(limNDigits+1,limPower+1)
  var s = (List.range(1, 10) map {k => (k -> Set[Long]())}).toMap
  s = s.updated(1, List.range(1L,10L).toSet)
  s = s.updated(2, List.range(10L,100L).toSet)
  s = s.updated(3, List.range(100L,1000L).toSet -- List.range(1,10).map{k => 111L*k}.toSet)
  var j = 1
  while (j <= limPower) {
    t(1)(j) = harmonicP(s(1), j)
    t(2)(j) = harmonicP(s(2), j)
    t(3)(j) = harmonicP(s(3), j)
    j = j + 1
  }
  println("t(1) : " + t(1).toList)
  println("t(2) : " + t(2).toList)
  println("t(3) : " + t(3).toList)
  List.range(4,mk) foreach {
    key => 
    val ns = s(key-1).map{
      x =>
      val toDel = if ((x%10) == (x/10)%10) Set(x%10) else Set[Long]()
      (s(1)+0L--toDel).map{ d => (x*10 + d) } 
     }.flatten
    s = s.updated(key, ns)
    j = 1
    while (j <= limPower) {
      t(key)(j) = harmonicP(s(key), j)
      j = j + 1
    }
  println("t(" + key + ") : " + t(key).toList)
  }
  var i = mk  	     
  var found = false
  while (i <= limNDigits && !found) {
    t(i)(1) = List.range(0,limPower).foldLeft(0.)( (r, c) => (r + coeffForNonConsec(1,c)*powerDigits(bound,c)*t(i - 1)(1 + c)))
    if (math.abs(t(i)(1)) < eps) found = true
    if (!found) {
      j = 2
      while (j <= limPower) {
	t(i)(j) = List.range(0,limPower-j+1).foldLeft(0.)( (r, c) => (r + coeffForNonConsec(j,c)*powerDigits(bound,c)*t(i - 1)(j + c)))
	j = j + 1
      }
    }
    if (i < 10 || i > limNDigits - 2) println("t(" + i + ") : " + t(i).toList)
    i = i + 1
  }
  println( "Time (s): " + ( System.currentTimeMillis() - start ) / 1000. )
  List.range(1,i).foldLeft(0.)(_ + t(_)(1))
}  

p368ForNonConsec(4,3,10,3)

def harmonicP(s: Set[Int], j: Int) = s.foldLeft(0.)(_+1./math.pow(_, j))

def p368ForNonConsec1(limNDigits: Int, limPower: Int) = {
  val start = System.currentTimeMillis
  val t: Array[Array[Array[Double]]] = new Array(1000,limNDigits+1,limPower+1)
  var s = (List.range(1, 10) map {k => (k -> Set[Int]())}).toMap
  s = s.updated(1, List.range(1,10).toSet)
  s = s.updated(2, List.range(10,100).toSet)
  s = s.updated(3, List.range(100,1000).toSet -- List.range(1,10).map{k => 111*k}.toSet)
  var j = 1
  while (j <= limPower) {
    s(1) foreach {
      el => 
	t(el)(3)(j) = harmonicP(s(3).filter(x => (x % 10) == el), j)
    }
    s(2) foreach {
      el => 
	t(el)(3)(j) = harmonicP(s(3).filter(x => (x % 100) == el), j)
    }
    s(3) foreach {
      el => 
	t(el)(3)(j) = harmonicP(s(3).filter(x => (x % 1000) == el), j)
    }
    j = j + 1
  }
  var i = 4  	     
  while (i <= limNDigits) {
      j = 1
      while (j <= limPower) {
	s(1) foreach {
	  d => 
	    val tpred = s(3).foldLeft(0.)(_+t(_)(i-1)(j))
	    t(d)(i)(j) = List.range(0, limPower-j+1).foldLeft(0.)( (r, c) => (r + coeffForNonConsec(j,c) * math.pow(d,c) * tpred))
	}
	(s(2) ++ s(3)) foreach {
	  k => 
	    t(k)(i)(j) = List.range(0, limPower-j+1).foldLeft(0.)( (r, c) => (r + coeffForNonConsec(j,c) * math.pow((k % 10),c) * t(k / 10)(i-1)(j)))
	}
	j = j + 1
      }
    i = i + 1
  }
  println( "Time (s): " + ( System.currentTimeMillis() - start ) / 1000. )
  harmonicP(s(1)++s(2), 1) + List.range(1,i).foldLeft(0.)((r,c) => (r + s(3).foldLeft(0.)((r1, c1) => (r1 + t(c1)(c-1)(1))) ))
}  

p368ForNonConsec1(4,3)


def p373(lim: Int) = {
  val start = System.currentTimeMillis
  //val lim = math.pow(10,k).toInt
  val file = "/media/ubuntuPart2/docs/topcoder/pythagorean-triples.txt"
  val rf: Input = Resource.fromFile(file)

  val lines = rf.lines().filter(_!="")
  val nlines = lines.size
  println("n lines = " + nlines)
  var s = 0L
  var clines = 0

  lines foreach {
    li => 
      clines = clines + 1
      val tmp = li.trim.split(" ").filter(_!="") map {_.toInt}
      if (tmp.length >= 3) {
	val ctriple = tmp.take(3)  
	val c = ctriple(2)
	/* if abc form a right triangle with c as hypothenuse, 
	 * R = c/2, so we need to look through all c < 2*lim
	 * each kc is a good radius, with k = 1 to lim/c
	 * so their sum is c*kc*(kc+1)/2
	 */
	val radius = c / 2
	val kc = lim / c
	if (radius <= lim) {
	//if (clines < 500 || clines >= (nlines - 1)) println("last trip = " + ctriple.toList + " gives naux = " + naux)
	val ns = c*kc*(kc+1)/2
	if (kc!=0) println(" found c " + c + " kc = " + kc + " ns = " + ns + " radii = " + (List.range(1,kc+1) map {c * _})  )
	s = s + ns
	}
	/* here we take the case of isosceles triangles b = c.
	 * we get:
	 * a = 2Rsin(pi-alpha)
	 * b = 2Rsin(alpha/2)
	 * with alpha the angle boc.
	 * using sin(pi-alpha) = sin(alpha)
	 * and sin(alpha) = 2sin(alpha/2)cos(alpha/2)
	 * we get that sin(alpha/2) = p/q with p < q both NAT
	 * from the eq with b, substituting cos(alpha/2)
	 * we get 4R*p*sqrt(q^2-p^2)/q^2
	 * so we look through phytagorean triples and the above "c"
	 * plays the role of q.
	 * we get that 4R = q^2*k with k from 1 to kc, kc = lim/(4q^2)
	 * so again the sum of all radii is c^2*kc(kc+1)/2
	 */ 
	val kc1 = lim/(4*c*c)
	val ns1 = 4*c*c*kc1*(kc1+1)/2
	if (kc1!=0) {
	  println(" found c " + c + " kc1 = " + kc1 + " ns1 = " + ns1 + " radii = " + (List.range(1,kc1+1) map {4 * c * c * _})  )
	  s = s + ns1
	}
      }
  }
  println( "Result: " + s)//goodTrips) 
  println( "Time (s): " + ( System.currentTimeMillis() - start ) / 1000. )
  s 
}

p373(100)

def p373All(lim: Int) = {
  val start = System.currentTimeMillis
  //val lim = math.pow(10,k).toInt
  val file = "/media/ubuntuPart2/docs/topcoder/pythagorean-triples.txt"
  val rf: Input = Resource.fromFile(file)

  val lines = rf.lines().filter(_!="")
  val nlines = lines.size
  println("n lines = " + nlines)
  var s = 0L
  var clines = 0
  val linesSplit = lines.toList map { li => li.trim.split(" ").filter(_!="") map {_.toInt} }
  val linesFilter1 = linesSplit.filter(_.length >= 3)
  val triples = linesFilter1.filter(t => t(2) <= 2*lim)
  val rs1 = for (t <- triples; val k = lim / t(2)) yield List.range(1,k+1).map{1L*t(2)*_}//1L*t(2)*k*(k+1)/2
  val rs2 = for (t1 <- triples; t2 <- triples; 
		 val factor = (t1(0)*t2(1) + t1(1)*t2(0)) ; 
		 val q1q2 = (t1(2) * t2(2)); 
		 val k = lim * factor / q1q2;
		 val gk = List.range(1,k+1).filter(kc => ((kc*q1q2) % factor == 0)); if (gk!=List())) yield gk.map{_*q1q2/factor}

 println( "Result: " + rs1.filter(_!=List()) + "\n" + rs2.filter(_!=List()))//goodTrips) 
  println( "Time (s): " + ( System.currentTimeMillis() - start ) / 1000. )
  //(rs1.foldLeft(0L)(_+_), rs2.foldLeft(0L)(_+_))
}

p373All(100)

p373All(1000000)

def p373NaiveBad(lim: Int) = {
  val start = System.currentTimeMillis
  //val lim = math.pow(10,k).toInt
  val file = "/media/ubuntuPart2/docs/topcoder/pythagorean-triples.txt"
  val rf: Input = Resource.fromFile(file)

  val lines = rf.lines().filter(_!="")
  val nlines = lines.size
  println("n lines = " + nlines)
  var s = 0L
  var clines = 0
  val linesSplit = lines.toList map { li => li.trim.split(" ").filter(_!="") map {_.toInt} }
  val linesFilter1 = linesSplit.filter(_.length >= 3)
  val triples = linesFilter1.filter(t => t(2) <= 2*lim)
  val triples2 = triples
  val rs = for (t1 <- triples; t2 <- triples2.drop(triples.indexOf(t1)); radius <- List.range(1,lim+1);
		 val factor1 = (t1(0)*t2(1) + t1(1)*t2(0)) ; 
		 val factor2 = (t1(1)*t2(1) + t1(0)*t2(0)) ; 
		 val q1q2 = (t1(2) * t2(2)); 
		 val condA = (radius % t1(2) == 0);
		 val condB = (radius % t2(2) == 0);
		 val condC1 = ((2*radius*factor1 % q1q2) == 0); 
		 val condC2 = ((2*radius*factor2 % q1q2) == 0);
		 val a = (if (condC1) 2*radius*t1(0)/t1(2) else  2*radius*t1(1)/t1(2));
		 val b = (if (condC1) 2*radius*t2(0)/t2(2) else  2*radius*t2(1)/t2(2));
		 val c = (if (condC1) 2*radius*factor1/q1q2 else  2*radius*factor2/q1q2);
		 if ( factor1!=0 && factor2!=0 && condA && condB && (condC1 || condC2)) )      
	   yield (radius, t1.toList.take(3), t2.toList.take(3), 
		  factor1, factor2, 
		  (a,b,c), (getRadius(a,b,c) == radius.toDouble)
		 )

 val rss = rs.sortBy(el => el._1)
 val rs1 = (rs map {_._1})
 val ss = rs1.toSet.toList.sorted
 s = rs1.foldLeft(0)(_+_)
 println( "Result: " + rss.take(40).foldLeft("")(_+"\n"+_) + "\nss = " + ss + " sumDistinct = " + ss.foldLeft(0)(_+_) + " sum = " + s)
 val bad = rs.filter(p => getRadius(p._6._1, p._6._2, p._6._3)!=p._1.toDouble)
 println(" exists bad " + bad + " sum without bad = " + (s - bad.foldLeft(0)(_+_._1)))
 println( "Time (s): " + ( System.currentTimeMillis() - start ) / 1000. )
  //(rs1.foldLeft(0L)(_+_), rs2.foldLeft(0L)(_+_))
}

def getRadius(a: Long, b: Long, c: Long) = {
  val abc = 1L*a*b*c
  val s = (a + b + c)/2
  val area = math.sqrt(1L*s*(s-a)*(s-b)*(s-c))
  val radius = abc / (4L*area)
  println("abc = " + abc + " area = " + area + " radius = " + radius)
  radius
}

def p373Naive(lim: Int) = {
  val start = System.currentTimeMillis
  //val lim = math.pow(10,k).toInt
  val file = "/media/ubuntuPart2/docs/topcoder/pythagorean-triples.txt"
  val rf: Input = Resource.fromFile(file)
  val lines = rf.lines().filter(_!="")
  val nlines = lines.size
  println("n lines = " + nlines)
  var s = 0L
  var clines = 0
  val linesSplit = lines.toList map { li => li.trim.split(" ").filter(_!="") map {_.toInt} }
  val linesFilter1 = linesSplit.filter(_.length >= 3)
  val triples = linesFilter1.filter(t => t(2) <= 3*lim)
  val n = triples.length
  println("triples length = " + n)
  var mR = Map[Int, Set[List[Int]]]().empty
  List.range(5, lim+1) foreach {i => mR = mR.updated(i, Set[List[Int]]())}
  var i = 0 
  var j = 1
  var radius = 5
  while (radius <= lim) {
    i = 0
    while (i < n) {
      val t = triples(i).toList.take(3)      
      //if (i == 2) println("radius = " + radius + " t = " + t.toList)
      /* check if possible isosceles triangle
       * a = b = 2R p/q
       * c = 2R ( 2p sqrt(q^2-p^2) / q^2 )
       */ 
      if ((radius % t(2)) == 0 && ((1L*radius*t(0)*t(1)) % (t(2)*t(2))) == 0 ) {
	val a1 = 2*radius*t(1)/t(2)
	val a2 = 2*radius*t(0)/t(2)
	val c = 4*radius*t(1)*t(0)/(t(2)*t(2))
	if (getRadius(a1, a1, c) == radius.toDouble) {
	  //println("found radius = " + radius + " for triple = " + t + ". Corr. *isosceles* triangle: (a, b, c) = " + (a1,a1,c)  + " check radius = " + (getRadius(a1,a1,c) == radius.toDouble))	    
	  mR = mR.updated(radius, mR(radius) + List(a1,a1,c).sorted)
	}
	else if (radius < lim) println("isosceles: not an integer triangle " + (a1,a1,c) + " for radius = " + radius + " triple = " + t)
	if (getRadius(a2, a2, c) == radius.toDouble) {
	  //println("found radius = " + radius + " for triple = " + t + ". Corr. *isosceles* triangle: (a, b, c) = " + (a2,a2,c)  + " check radius = " + (getRadius(a2,a2,c) == radius.toDouble))	    
          //lR = (radius,List(a2,a2,c))+:lR
	  mR = mR.updated(radius, mR(radius) + List(a2,a2,c).sorted)
	}
	else if (radius < lim) println("isosceles: not an integer triangle " + (a2,a2,c) + " for radius = " + radius + " triple = " + t)
	//else println("not an integer triangle " + (a2,a2,c) + " for radius = " + radius)
      }
      /* check if possible right triangle
       * a = 2R b = 2R p/q
       * c = 2R ( sqrt(q^2-p^2) / q )
       */ 
      if ((radius % t(2)) == 0) {
	val a = 2*radius*t(1)/t(2)
	val c = 2*radius*t(0)/t(2)
	val b = 2*radius
	//println("found radius = " + radius + " for triple = " + t + ". Corr. *right* triangle: (a, b, c) = " + (a,b,c)  + " check radius = " + (getRadius(a,b,c) == radius.toDouble))	    
	mR = mR.updated(radius, mR(radius) + List(a,b,c).sorted)
      }
      /* the general case:
       * a = 2R p1/q1
       * b = 2R p2/q2
       * c = 2R (p1*sqrt(q2^2 - p2^2) + p2*sqrt(q1^2 - p1^2)) / q1q2
       */
      j = i + 1
      while (j < n) {
	val tp = triples(j).toList.take(3)
	val factor1 = t(0)*tp(1) + t(1)*tp(0)
	val factor2 = t(1)*tp(1) + t(0)*tp(0)
	val q1q2 = t(2)*tp(2)
	val cond1 = (radius*factor1) % q1q2 == 0
	val cond2 = (radius*factor2) % q1q2 == 0
	if ( ( (radius % t(2)) == 0 ) && ( (radius % tp(2)) == 0 ) && (cond1 || cond2) ) {
	    var a = 2*radius*t(0)/t(2)
	    var b = 2*radius*tp(0)/tp(2)
	    var c = 2*radius*factor1/q1q2
	    if (cond1) {
	      if (getRadius(a,b,c) == radius.toDouble) {
		//println("found radius = " + radius + " for triple1 = " + t + " and triple2 = " + tp + ". Corr. *general1* triangle: (a, b, c) = " + (a,b,c) + " check radius = " + (getRadius(a,b,c) == radius.toDouble))
		mR = mR.updated(radius, mR(radius) + List(a,b,c).sorted)
	      }	  
	      else if (radius < lim) println("1:not an integer triangle " + (a,b,c) + " for radius = " + radius + " triples = " + t + ", " + tp )
	    }
	    a = 2*radius*t(1)/t(2)
	    b = 2*radius*tp(1)/tp(2)
	    c = 2*radius*factor1/q1q2
	    if (cond1) {
	      if (getRadius(a,b,c) == radius.toDouble) {
		//println("found radius = " + radius + " for triple1 = " + t + " and triple2 = " + tp + ". Corr. *general2* triangle: (a, b, c) = " + (a,b,c) + " check radius = " + (getRadius(a,b,c) == radius.toDouble))	    
		mR = mR.updated(radius, mR(radius) + List(a,b,c).sorted)
	      }
	      else if (radius < lim) println("2:not an integer triangle " + (a,b,c) + " for radius = " + radius + " triple = " + t + ", " + tp )
	    }
	    //else println("not an integer triangle " + (a,b,c) + " for radius = " + radius)
	    a = 2*radius*t(0)/t(2)
	    b = 2*radius*tp(1)/tp(2)
	    c = 2*radius*factor2/q1q2
	    if (cond2) {
	      if (getRadius(a,b,c) == radius.toDouble) {
		//println("found radius = " + radius + " for triple1 = " + t + " and triple2 = " + tp + ". Corr. *general3* triangle: (a, b, c) = " + (a,b,c) + " check radius = " + (getRadius(a,b,c) == radius.toDouble))	    
		mR = mR.updated(radius, mR(radius) + List(a,b,c).sorted)
	      }
	      //else println("not an integer triangle " + (a,b,c) + " for radius = " + radius)
	      else if (radius < lim) println("3:not an integer triangle " + (a,b,c) + " for radius = " + radius + " triple = " + t + ", " + tp )
	    }
	    a = 2*radius*t(1)/t(2)
	    b = 2*radius*tp(0)/tp(2)
	    c = 2*radius*factor2/q1q2
	    if (cond2) {
	      if (getRadius(a,b,c) == radius.toDouble) {
		//println("found radius = " + radius + " for triple1 = " + t + " and triple2 = " + tp + ". Corr. *general4* triangle: (a, b, c) = " + (a,b,c) + " check radius = " + (getRadius(a,b,c) == radius.toDouble))	    
		mR = mR.updated(radius, mR(radius) + List(a,b,c).sorted)
	      }
	      //else println("not an integer triangle " + (a,b,c) + " for radius = " + radius)
	      else if (radius < lim) println("4:not an integer triangle " + (a,b,c) + " for radius = " + radius + " triple = " + t + ", " + tp )
	    }
	}
	j = j + 1
      }
      i = i + 1
    }
    radius = radius + 1
  }

  val mRF = mR.filter(_._2.size!=0)
  println( "Result: " + mRF.takeRight(20).foldLeft("")(_+"\n"+_) + "\n s = " )
  println( "Time (s): " + ( System.currentTimeMillis() - start ) / 1000. )
  mRF.foldLeft(0)((r, c) => ( r + c._1*c._2.size))
}

p373Naive(1200)

def p373Naive(lim: Int) = {
  val start = System.currentTimeMillis
  val file = "/media/ubuntuPart2/docs/topcoder/pythagorean-triples.txt"
  val rf: Input = Resource.fromFile(file)
  val lines = rf.lines().filter(_!="")
  val nlines = lines.size
  println("n lines = " + nlines)
  var s = 0L
  var clines = 0
  val linesSplit = lines.toList map { li => li.trim.split(" ").filter(_!="") map {_.toInt} }
  val linesFilter1 = linesSplit.filter(_.length >= 3)
  val triples = linesFilter1.filter(t => t(2) <= 2*lim)
  val n = triples.length
  println("triples length = " + n)
  var mR = Map[Int, Set[List[Int]]]().empty
  List.range(5, lim+1) foreach {i => mR = mR.updated(i, Set[List[Int]]())}
  var i = 0 
  var j = 1
  var radius = 5
  while (radius <= lim) {
    i = 0
    while (i < n) {
      val t = triples(i).toList.take(3)      
      //if (i == 2) println("radius = " + radius + " t = " + t.toList)
      if ((radius % t(2)) == 0) { 
      /* right triangle
       * a = 2R b = 2R p/q
       * c = 2R ( sqrt(q^2-p^2) / q )
       */ 
	val a = 2*radius*t(1)/t(2)
	val c = 2*radius*t(0)/t(2)
	val b = 2*radius
	mR = mR.updated(radius, mR(radius) + List(a,b,c).sorted)
      /* check if possible isosceles triangle
       * a = b = 2R p/q
       * c = 2R ( 2p sqrt(q^2-p^2) / q^2 )
       */ 	
	if (((1L*radius*t(0)*t(1)) % (t(2)*t(2))) == 0 ) {
	  val a1 = 2*radius*t(1)/t(2)
	  val a2 = 2*radius*t(0)/t(2)
	  val c = 4*radius*t(1)*t(0)/(t(2)*t(2))
	  mR = mR.updated(radius, mR(radius) + List(a1,a1,c).sorted + List(a2,a2,c).sorted)
	}
	/* the general case:
	 * a = 2R p1/q1
	 * b = 2R p2/q2
	 * c = 2R (p1*sqrt(q2^2 - p2^2) + p2*sqrt(q1^2 - p1^2)) / q1q2
	 */
	j = i + 1
	while (j < n) {
	  val tp = triples(j).toList.take(3)
	  if ( (radius % tp(2)) == 0 ) {
	    val q1q2 = t(2)*tp(2)
	    val a1 = 2*radius*t(0)/t(2)
	    val b1 = 2*radius*tp(0)/tp(2)
	    val a2 = 2*radius*t(1)/t(2)
	    val b2 = 2*radius*tp(1)/tp(2)
	    val factor1 = t(0)*tp(1) + t(1)*tp(0)
	    val cond1 = (radius*factor1) % q1q2 == 0
	    if (cond1) {
	      val c = 2*radius*factor1/q1q2
	      mR = mR.updated(radius, mR(radius) + List(a1,b1,c).sorted + List(a2,b2,c).sorted )
	    }
	    val factor2 = t(1)*tp(1) + t(0)*tp(0)
	    val cond2 = (radius*factor2) % q1q2 == 0
	    if (cond2) {
	      val c = 2*radius*factor2/q1q2
    	      mR = mR.updated(radius, mR(radius) + List(a1,b2,c).sorted + List(a2,b1,c).sorted )
	    }
	  }
	  j = j + 1
	}
      }
      i = i + 1
    }
    radius = radius + 1
  }

  val mRF = mR.filter(_._2.size!=0)
  println( "Result: " + mRF.takeRight(20).foldLeft("")(_+"\n"+_) + "\n s = " )
  println( "Time (s): " + ( System.currentTimeMillis() - start ) / 1000. )
  mRF.foldLeft(0)((r, c) => ( r + c._1*c._2.size))
}

def p373Naive(lim: Int) = {
  val start = System.currentTimeMillis
  val file = "/media/ubuntuPart2/docs/topcoder/pythagorean-triples.txt"
  val rf: Input = Resource.fromFile(file)
  val lines = rf.lines().filter(_!="")
  val nlines = lines.size
  println("n lines = " + nlines)
  var s = 0L
  var clines = 0
  val linesSplit = lines.toList map { li => li.trim.split(" ").filter(_!="") map {_.toInt} }
  val linesFilter1 = linesSplit.filter(_.length >= 3)
  val triples = linesFilter1.filter(t => t(2) <= 2*lim)
  val n = triples.length
  println("triples length = " + n)
  var mR = Map[Int, Set[List[Int]]]().empty
  List.range(5, lim+1) foreach {i => mR = mR.updated(i, Set[List[Int]]())}
  var i = 0 
  var j = 1
  var radius = 5
  while (radius <= lim) {
    i = 0
    while (i < n) {
      val t = triples(i).toList.take(3)      
      //if (i == 2) println("radius = " + radius + " t = " + t.toList)
      if ((radius % t(2)) == 0) { 
      /* right triangle
       * a = 2R b = 2R p/q
       * c = 2R ( sqrt(q^2-p^2) / q )
       */ 
	val a = (radius/t(2))*2*t(1)
	val c = (radius/t(2))*2*t(0)
	val b = 2*radius
	mR = mR.updated(radius, mR(radius) + List(a,b,c).sorted)
	println("added " + (a,b,c))
      /* check if possible isosceles triangle
       * a = b = 2R p/q
       * c = 2R ( 2p sqrt(q^2-p^2) / q^2 )
       */ 	
	if (((radius*t(0)*t(1)) % (t(2)*t(2))) == 0 ) {
	  val a1 = (radius/t(2))*2*t(1)
	  val a2 = (radius/t(2))*2*t(0)
	  val c = (radius/(t(2)*t(2)))*4*t(1)*t(0)
	  mR = mR.updated(radius, mR(radius) + List(a1,a1,c).sorted + List(a2,a2,c).sorted)
	  println("added isosceles " + (a1,a1,c) + " and " + (a2,a2,c))
	}
	/* the general case:
	 * a = 2R p1/q1
	 * b = 2R p2/q2
	 * c = 2R (p1*sqrt(q2^2 - p2^2) + p2*sqrt(q1^2 - p1^2)) / q1q2
	 */
	j = i + 1
	while (j < n) {
	  val tp = triples(j).toList.take(3)
	  if ( (radius % tp(2)) == 0 ) {
	    val q1q2 = t(2)*tp(2)
	    val a1 = (radius/t(2))*2*t(0)
	    val b1 = (radius/tp(2))*2*tp(0)
	    val a2 = (radius/t(2))*2*t(1)
	    val b2 = (radius/tp(2))*2*tp(1)
	    val factor1 = t(0)*tp(1) + t(1)*tp(0)
	    val cond1 = (radius*factor1) % q1q2 == 0
	    if (cond1) {
	      //careful: radius is not necessarily multiple of q1q2 as it may just as well be that factor1 contains some factors!!
	      //val c = (radius/q1q2)*2*factor1
	      val c = 2*radius*factor1/q1q2
	      mR = mR.updated(radius, mR(radius) + List(a1,b1,c).sorted + List(a2,b2,c).sorted )
	      println("added " + (a1,b1,c) + " and " + (a2,b2,c))
	    }
	    val factor2 = t(1)*tp(1) + t(0)*tp(0)
	    val cond2 = (radius*factor2) % q1q2 == 0
	    if (cond2) {
	      val c = 2*radius*factor2/q1q2
    	      mR = mR.updated(radius, mR(radius) + List(a1,b2,c).sorted + List(a2,b1,c).sorted )
	      println("added " + (a1,b2,c) + " and " + (a2,b1,c))
	    }
	  }
	  j = j + 1
	}
      }
      i = i + 1
    }
    radius = radius + 1
  }

  val mRF = mR.filter(_._2.size!=0)
  val s = mRF.foldLeft(0)((r, c) => ( r + c._1*c._2.size))
  println( "Result: " + mRF.takeRight(20).foldLeft("")(_+"\n"+_) + "\n s = " + s)
  println( "Time (s): " + ( System.currentTimeMillis() - start ) / 1000. )
  mRF
}

p373Naive(1200)

def lcm(a: Int, b: Int) = a * (b / gcd(a, b))

def p373Fin(lim: Int) = {
  val start = System.currentTimeMillis
  val file = "/media/ubuntuPart2/docs/topcoder/pythagorean-triples.txt"
  val rf: Input = Resource.fromFile(file)
  val lines = rf.lines().filter(_!="")
  val nlines = lines.size
  println("n lines = " + nlines)
  var s = 0L
  var clines = 0
  val linesSplit = lines.toList map { li => li.trim.split(" ").filter(_!="") map {_.toInt} }
  val linesFilter1 = linesSplit.filter(_.length >= 3)
  val triples = linesFilter1.filter(t => t(2) <= 2*lim)
  val n = triples.length
  println("triples length = " + n)
  var i = 0 
  var j = 1
  var abc = List[List[Int]]()
  while (i < n) {
    val t = triples(i).toList.take(3)      
    /* right triangle
     * a = 2R b = 2R p/q
     * c = 2R ( sqrt(q^2-p^2) / q )
     * SO R = kq with k <= lim/q, i.e., the sum of radii is q*kc*(kc+1)/2
     * with kc = lim/q
     */ 
    val kcRight = lim/t(2)
    s = s + t(2) * kcRight * (kcRight + 1)/2
    /* isosceles triangle
     * a = b = 2R p/q or a = b = 2R r/q
     * c = 2R ( 2p sqrt(q^2-p^2) / q^2 )
     * SO R = kq^2 with k <= lim/q^2, i.e., the sum of radii is 2*q^2*kc*(kc+1)/2
     * with kc = lim/q^2
     * we have twice because there are 2 isosceles triangles possible
     */ 	
    val kcIsosceles = lim/(t(2)*t(2))
    s = s + t(2) * t(2) * kcIsosceles * (kcIsosceles + 1)
    /* the general case:
     * a = 2R p1/q1
     * b = 2R p2/q2
     * c = 2R (p1*sqrt(q2^2 - p2^2) + p2*sqrt(q1^2 - p1^2)) / q1q2
     */
    j = i + 1
    while (j < n) {
      val tp = triples(j).toList.take(3)
      val q1q2 = t(2)*tp(2)
      val mq1q2 = lcm(t(2), tp(2))
      val q2p1 = tp(2)*t(0)
      val q1p2 = t(2)*tp(0) 
      val q2r1 = tp(2)*t(1)
      val q1r2 = t(2)*tp(1) 
      if (q2p1 != q1p2 || q2r1 != q1r2) {	
	val factor1 = t(0)*tp(1) + t(1)*tp(0)
	val d1 = gcd(q1q2, factor1)
	val r1 = q1q2 / d1
	val l1 = lcm(t(2), lcm(tp(2), r1))
	val kc1 = lim / mq1q2 //l1
	val ns = r1 * kc1 * (kc1 + 1) / 2
	val goodK1 = List.range(1, kc1+1)
	if (!(q2p1 == q1p2 || q2p1 == factor1 || q1p2 == factor1) ) {
	  s = s + ns
	  abc = abc ::: (goodK1 map {k => List(2*k*q2p1/d1, 2*k*q1p2/d1, 2*k*factor1/d1)})
	}
	if (q2r1 != q1r2  && q2p1 != factor1 && q1p2 != factor1) {
	  s = s + ns
	  abc = abc ::: (goodK1 map {k => List(2*k*q2r1/d1, 2*k*q1r2/d1, 2*k*factor1/d1)})
	}
      }
      if (q2p1 != q1r2 || q2r1 != q1p2) {	
	val factor2 = t(1)*tp(1) + t(0)*tp(0)
	val d2 = gcd(q1q2, factor2)
	val r2 = q1q2 / d2
	//val l2 = lcm(mq1q2, r2)
	val l2 = lcm(t(2), lcm(tp(2), r2))
	val kc2 = lim / mq1q2//l2
	val goodK2 = List.range(1,kc2+1)
	val ns = r2 * kc2 * (kc2 + 1) / 2
	if (q2p1 != q1r2 && q2p1 != factor2 && q1r2 != factor2) {
	  s = s + ns
	  abc = abc ::: (goodK2 map {k => List(2*k*q2p1/d2, 2*k*q1r2/d2, 2*k*factor2/d2)})
	}
	if (q2r1 != q1p2 && q2r1 != factor2 && q1p2 != factor2) {
	  s = s + ns
	  abc = abc ::: (goodK2 map {k => List(2*k*q2r1/d2, 2*k*q1p2/d2, 2*k*factor2/d2)})
	}
      }      
      j = j + 1
    }
  i = i + 1
  }

  println( "Result: " + s)
  println( "Time (s): " + ( System.currentTimeMillis() - start ) / 1000. )
  abc
}

val abc = p373Fin(100)

abc map {l => getRadius(l(0),l(1),l(2))}

def p373Fin(lim: Int) = {
  val start = System.currentTimeMillis
  val file = "/media/ubuntuPart2/docs/topcoder/pythagorean-triples.txt"
  val rf: Input = Resource.fromFile(file)
  val lines = rf.lines().filter(_!="")
  val nlines = lines.size
  println("n lines = " + nlines)
  var s = 0L
  var clines = 0
  val linesSplit = lines.toList map { li => li.trim.split(" ").filter(_!="") map {_.toInt} }
  val linesFilter1 = linesSplit.filter(_.length >= 3)
  val triples = linesFilter1.filter(t => t(2) <= 2*lim)
  val n = triples.length
  println("triples length = " + n)
  var i = 0 
  var j = 1
  var abc = List[List[Double]]()
  val triangles: Array[Array[Array[Int]]] = new Array(2*lim+1, 2*lim+1, 2*lim+1)
  while (i < n) {
    val t = triples(i).toList.take(3)      
    /* right triangle
     * a = 2R b = 2R p/q
     * c = 2R ( sqrt(q^2-p^2) / q )
     * SO R = kq with k <= lim/q, i.e., the sum of radii is q*kc*(kc+1)/2
     * with kc = lim/q
     */ 
    val kcRight = lim/t(2)
    s = s + t(2) * kcRight * (kcRight + 1)/2
    /* isosceles triangle
     * a = b = 2R p/q or a = b = 2R r/q
     * c = 2R ( 2p sqrt(q^2-p^2) / q^2 )
     * SO R = kq^2 with k <= lim/q^2, i.e., the sum of radii is 2*q^2*kc*(kc+1)/2
     * with kc = lim/q^2
     * we have twice because there are 2 isosceles triangles possible
     */ 	
    val kcIsosceles = lim/(t(2)*t(2))
    s = s + t(2) * t(2) * kcIsosceles * (kcIsosceles + 1)
    /* the general case:
     * a = 2R p1/q1
     * b = 2R p2/q2
     * c = 2R (p1*sqrt(q2^2 - p2^2) + p2*sqrt(q1^2 - p1^2)) / q1q2
     */
    j = i + 1
    while (j < n) {
      val tp = triples(j).toList.take(3)
      val q1q2 = t(2)*tp(2)
      val mq1q2 = lcm(t(2), tp(2))
      val q2p1 = tp(2)*t(0)
      val q1p2 = t(2)*tp(0) 
      val q2r1 = tp(2)*t(1)
      val q1r2 = t(2)*tp(1) 
      if (q2p1 != q1p2 || q2r1 != q1r2) {	
	val factor1 = t(0)*tp(1) + t(1)*tp(0)
	val d1 = gcd(q1q2, factor1)
	val r1 = q1q2 / d1
	val l1 = lcm(t(2), lcm(tp(2), r1))
	val kc1 = lim / l1
	val ns = l1 * kc1 * (kc1 + 1) / 2
	val goodK1 = List.range(1, kc1+1)
	//if (!(q2p1 == q1p2 || q2p1 == factor1 || q1p2 == factor1) ) {     
	//if (l1*t(0)/t(2) != l1*tp(0)/tp(2) && l1*tp(0)/tp(2) != l1*factor1/q1q2 && l1*factor1/q1q2 != l1*t(0)/t(2) ) {
	var a = l1*t(0)/t(2)
	var b = l1*tp(0)/tp(2)
	var c = l1*factor1/q1q2
	if (kc1 != 0 && a != b && b != c && c != a) {
	  val l = List(a,b,c).sorted
	  if (triangles(l(0))(l(1))(l(2)) == 0) { 
	    triangles(l(0))(l(1))(l(2)) = 1
	    //println("abc = " + (a,b,c) + " t = " + t + " tp = " + tp + " l1 = " + l1 + " factor1 = " + factor1 + " kc1 = " + kc1 + " d1 = " + d1)
	    s = s + ns
	    //abc = abc ::: (goodK1 map {k => List(2.*k*l1*t(0)/t(2), 2.*k*l1*tp(0)/tp(2), 2.*k*l1*factor1/q1q2)})
	  }
	}
	//if (q2r1 != q1r2  && q2p1 != factor1 && q1p2 != factor1) {
	//if (l1*t(1)/t(2) != l1*tp(1)/tp(2) && l1*factor1/q1q2 !=  l1*tp(1)/tp(2) &&  l1*factor1 != l1*t(1)/t(2) ) {
	a = l1*t(1)/t(2)
	b = l1*tp(1)/tp(2)
	if (kc1 != 0 && a != b && b != c && c != a) {
	  val l = List(a,b,c).sorted
	  if (triangles(l(0))(l(1))(l(2)) == 0) { 
	    triangles(l(0))(l(1))(l(2)) = 1
	    //println("abc = " + (a,b,c) + " t = " + t + " tp = " + tp + " l1 = " + l1 + " factor1 = " + factor1 + " kc1 = " + kc1 + " d1 = " + d1)
	    s = s + ns
	    //abc = abc ::: (goodK1 map {k => List(2.*k*l1*t(1)/t(2), 2.*k*l1*tp(1)/tp(2), 2*k*l1*factor1/q1q2)})
	  }
	}
      }
      if (q2p1 != q1r2 || q2r1 != q1p2) {	
	val factor2 = t(1)*tp(1) + t(0)*tp(0)
	val d2 = gcd(q1q2, factor2)
	val r2 = q1q2 / d2
	val l2 = lcm(mq1q2, r2)
	val kc2 = lim / l2
	val goodK2 = List.range(1,kc2+1)
	val ns = l2 * kc2 * (kc2 + 1) / 2
	//if (q2p1 != q1r2 && q2p1 != factor2 && q1r2 != factor2) {
	//if (l2*t(0)/t(2) != l2*tp(1)/tp(2) && l2*tp(1)/tp(2) != l2*factor2/q1q2 &&  l2*factor2/q1q2 != l2*t(0)/t(2) ) {
	var a = l2*t(0)/t(2)
	var b = l2*tp(1)/tp(2)
	val c = l2*factor2/q1q2
	if (kc2 != 0 && a != b && b != c && c != a) {
	  val l = List(a,b,c).sorted
	  if (triangles(l(0))(l(1))(l(2)) == 0) { 
	    triangles(l(0))(l(1))(l(2)) = 1
	    //println("abc = " + (a,b,c) + " t = " + t + " tp = " + tp + " l2 = " + l2 + " factor2 = " + factor2 + " kc2 = " + kc2 + " d2 = " + d2)
	    s = s + ns
	    //abc = abc ::: (goodK2 map {k => List(2.*k*l2*t(0)/t(2), 2.*k*l2*tp(1)/tp(2), 2.*k*l2*factor2/q1q2)})
	  }
	}
	//if (q2r1 != q1p2 && q2r1 != factor2 && q1p2 != factor2) {
	a = l2*t(1)/t(2) 
	b = l2*tp(0)/tp(2)
	if (kc2 != 0 && a != b && b != c && c != a) {
	  val l = List(a,b,c).sorted
	  if (triangles(l(0))(l(1))(l(2)) == 0) { 
	    triangles(l(0))(l(1))(l(2)) = 1
	    //println("abc = " + (a,b,c) + " t = " + t + " tp = " + tp + " l2 = " + l2 + " factor2 = " + factor2 + " kc2 = " + kc2 + " d2 = " + d2)
	    s = s + ns
	    //abc = abc ::: (goodK2 map {k => List(2.*k*l2*t(1)/t(2), 2.*k*l2*tp(0)/tp(2), 2.*k*l2*factor2/q1q2)})
	  }
	}
      }      
      j = j + 1
    }
  i = i + 1
  }

  println( "Result: " + s)
  println( "Time (s): " + ( System.currentTimeMillis() - start ) / 1000. )
  //abc
}

val abc = p373Fin(100)

//ways to combine coins to sum s

def nways(s : Int, coeffs : List[Int]) = {
val r = 
for(a0 <- List.range(0, s/coeffs(7) + 1); 
      a1 <- List.range(0, s/coeffs(6) + 1); 
	a2 <- List.range(0, s/coeffs(5) + 1); 
	  a3 <- List.range(0, s/coeffs(4) + 1); 
	    a4 <- List.range(0, s/coeffs(3) + 1);
	      a5 <- List.range(0, s/coeffs(2) + 1);
		a6 <- List.range(0, s/coeffs(1) + 1);
		  a7 <- List.range(0, s/coeffs(0) + 1);
    if (a7*coeffs(0) + a6*coeffs(1) + a5*coeffs(2) + a4*coeffs(3) + a3*coeffs(4) + a2*coeffs(5) + a1*coeffs(6) + a0*coeffs(7) == s)
) yield 1

r.length

}

def nways(s : Int, coeffs : List[Int]) = {
var res = 0
for(
    a0 <- s to 0 by (0 - coeffs(7)); 
      a1 <- a0 to 0 by (0 - coeffs(6)); 
	a2 <- a1 to 0 by (0 - coeffs(5)); 
	  a3 <- a2 to 0 by (0 - coeffs(4)); 
	    a4 <- a3 to 0 by (0 - coeffs(3)); 
	      a5 <- a4 to 0 by (0 - coeffs(2)); 
		a6 <- a5 to 0 by (0 - coeffs(1))
) res += 1
res
}

nways(200, List(1, 2, 5, 10, 20, 50, 100, 200))

def nways(s : Int) = {
var res = 0
for(
    a0 <- s to 0 by -200; 
      a1 <- a0 to 0 by -100;
	a2 <- a1 to 0 by -50;
	  a3 <- a2 to 0 by -20;
	    a4 <- a3 to 0 by -10;
	      a5 <- a4 to 0 by -5; 
		a6 <- a5 to 0 by -2		 
) res += 1
res
}

nways(200)

//how many ways to change money with coins
//for 4 there are 3 ways: 1111, 112, 22
def countChange(money: Int, coins: List[Int]): Int = {
    //require(coins.length > 0, println("you cannot give change if you do not have any coins."))
    if (coins.length == 0) 0
    else if (coins.length == 1 && money % coins.head == 0) 1
    else if (money == 0) 1
    else {      
      val cand = List.range(0, money/coins.head+1)
      val res = cand.map{c => countChange(money - c*coins.head, coins.tail)}
      println("money = " + money + " coins = " + coins + " cand = " + cand + " res = " + res)
      res.foldLeft(0)(_+_)
      //cand.foldLeft(0)((r,c)=>(r + countChange(money - c*coins.head, coins.tail)))
    }
  }

countChange(4, List(1,2))

//test for p192

import scalax.io._
import scalax.file.{ FileOps, Path, NotFileException }

val file = "/media/ubuntuPart2/docs/topcoder/outP192_1000.txt"
val rf: Input = Resource.fromFile(file)
val lines = rf.lines().filter(_!="")
lines.foldLeft(BigInt(0))(_+_.trim.toLong)

//test for p64

import scalax.io._
import scalax.file.{ FileOps, Path, NotFileException }

val file = "/media/ubuntuPart2/docs/topcoder/odd-periodic-contFrac.txt"
val rf: Input = Resource.fromFile(file)
val line = rf.lines().filter(_!="")
line.split(",").toList.map{_.trim}.foldLeft(0L)(_+_.toLong)

//for problem 7
import scalax.io._
import scalax.file.{ FileOps, Path, NotFileException }

  val file = "/media/ubuntuPart2/docs/topcoder/primesSmallerThan10to6.txt"
  val rf: Input = Resource.fromFile(file)

  val lines = rf.lines().filter(_!="")
  val nlines = lines.size
  println("n lines = " + nlines)
  var clines = 0
  var nprimes = 0
  var lprimes = List[Long]()
  lines foreach {
    pli => 
      clines = clines + 1
      val cprimes = pli.trim.split(" ").filter(_!="").map(_.toLong).toList
      if(clines < 3) println(cprimes)
      nprimes += cprimes.length
      //lprimes = lprimes ::: cprimes  
      if (nprimes == 10001) println("ok = " + lprimes(10001))
  }

//for problem 10
import scalax.io._
import scalax.file.{ FileOps, Path, NotFileException }

  val file = "/media/ubuntuPart2/docs/topcoder/primesSmallerThan2Million.txt"
  val rf: Input = Resource.fromFile(file)

  val lines = rf.lines().filter(_!="")
  val nlines = lines.size
  println("n lines = " + nlines)
  var clines = 0
  var s = BigInt(0)
  lines foreach {
    pli => 
      clines = clines + 1
      val cprimes = pli.trim.split(" ").filter(_!="").map(_.toLong).toList
      if(clines < 3 || clines > 128866) println(cprimes)
      s += cprimes.foldLeft(BigInt(0))(_+_)      
  }

//for problem 14
def collatzLen(n: Int, l: Int): Long = if(n == 1) (l+1) else if (n % 2 == 0) collatzLen(n/2, l + 1) else collatzLen(3*n+1, l + 1)

//for problem 25

  def find(l: Int) = { 
    var i = 0
    var nf = 3
    var fib1 = BigInt(1)
    var fib2 = BigInt(1)
    var fib = BigInt(2)
    while (i < l) {
      fib = fib1 + fib2 
      fib1 = fib2
      fib2 = fib
      i = fib.toString.length
      //println("i = " + i + " nf= " + nf + " fib = " + fib)
      nf += 1
    }
    nf-1
  }





