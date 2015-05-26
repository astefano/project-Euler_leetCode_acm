object P109 {

val S = 1
val D = 2
val T = 3
val BULL = 25
val BULLEYE = 50

val sdt = Set(S, D, T)
val indices = List.range(1, 21) 

def isValid(v: Int) = v == 2*BULL || v == 2*BULLEYE || (v > 0 && (v % 2 == 0 && v / 2 <= 20))

def getV(n: Int) = n match {
  case S => "S"
  case D => "D"
  case T => "T"
}

def gen(n: Int) = {
  var nw = if (isValid(n)) 1 else 0
  //if (isValid(n)) println("D" + (n/2))

  var res = for(c1 <- sdt; c2 <- sdt; d1 <- indices; d2 <- indices; val rem = n - c1*d1 - c2*d2; if (isValid(rem))) yield (getV(c1)+d1, getV(c2)+d2, "D"+(rem/2))    

  indices foreach {
    d1 => 
      sdt foreach {
	c1 => 
	  val rem = (n - c1*d1)
	  if (isValid(rem)) {
	    //println(getV(c1) + d1 + ", D" + (rem/2))
	    nw += 1
	  }
	if (isValid(rem - BULL)) {
	  val d2 = rem - BULL
	  res = res + ((getV(c1)+d1, "D"+d2, "D25"))
	}
	if (isValid(rem - BULLEYE)) {
	  val d2 = rem - BULLEYE
	  res = res + ((getV(c1)+d1, "D"+d2, "D50"))
	}
      }
  }
  if (isValid(n - BULL)) nw += 1
  if (isValid(n - BULLEYE)) nw += 1
  //println("res = " + res)
  if (res.isEmpty) nw else {
  val w3 = res.tail.foldLeft(List(res.head))((r,c) => if (!(r.map(x => x._3).toSet)(c._3)) r :+ c else if (!(r.map(x => (x._1, x._2)).toSet((c._1, c._2)) || r.map(x => (x._2, x._1)).toSet((c._1, c._2)))) r :+ c else r)
  w3.length + nw
  }
}

def gen2(lim: Int) = {
  val scores = (List.range(1,21).flatMap{i => List(i, 2*i, 3*i)} :+ 25 :+ 50)//.toList
  val ns = scores.length
  val indices = List.range(0, ns)
  val doubles = List.range(1,21).map{i => 2*i} :+ 50 
  //each d in double counts for a checkout for value d
  var n = doubles.length

  indices foreach {
    i => 
      //count _, score, double
      n += doubles.takeWhile{x => x + scores(i) < lim}.length
      List.range(i, ns) foreach {
	j => 
	  n += doubles.takeWhile{x => x + scores(i) + scores(j) < lim}.length
      }
  }
  n
}

def main(args: Array[String]) {
  //val res = List.range(2, 171).reduceLeft(_+gen(_))

  val res = gen2(100)
  //val res = List.range(2, 100).reduceLeft(_+gen(_))
  //val res = gen(170)
  println("res = " + res)
}

}
