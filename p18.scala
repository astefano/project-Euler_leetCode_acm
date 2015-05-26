def fillTr(trFlat: List[List[Int]]) = {
      val n = trFlat.length
	val middle = n-1 
      val tr = Array.ofDim[Int](n, 2*n-1)
      val indices = List.range(0, n)
      indices foreach {
      i => 
      val nr = trFlat(i).length
      List.range(0, nr) foreach {
      j => 
	val k = math.abs(middle - i) + 2*j
      //require( k < 2*n+10, println("bad index at i = " + i + " j = " + j + " middle = " + middle + " k = " + k))	
      tr(i)(k) = trFlat(i)(j)
      }
      }
      tr.map{_.toList}.toList
      }

val res = fillTr(trFlat)

val trFlat = List(List(3), List(7,4), List(2,4,6), List(8,5,9,3))

def printTr(tr: Array[Array[Int]]) = tr.foldLeft("")(_+"\n"+_.map{x=> if (x<10) (" 0"+x.toString + " ") else (" " + x.toString + " ")}.mkString.replace("00"," "))

val trFlat = List(List(75), List(95,64), List(17,47,82), List(18,35,87,10), List(20,4,82,47,65), List(19,1,23,75,3,34), List(88,2,77,73,7,63,67), List(99,65,4,28,06,16,70,92), List(41,41,26,56,83,40,80,70,33), List(41,48,72,33,47,32,37,16,94,29), List(53,71,44,65,25,43,91,52,97,51,14), List(70,11,33,28,77,73,17,78,39,68,17,57), List(91,71,52,38,17,14,91,43,58,50,27,29,48), List(63,66,4,68,89,53,67,30,73,16,69,87,40,31), List(4,62,98,27,23,9,70,98,73,93,38,53,60,4,23))

val res = fillTr(trFlat)

printTr(res)

def findMax(tr: List[List[Int]]): Int = {
	val nrows = tr.length
	val middle = nrows-1 
	val s = Array.ofDim[Int](nrows, 2*nrows-1)
	s(0)(middle) = tr(0)(0)
	println("i = " + s(0).toList)
	val indices = List.range(1, nrows-1)
	indices foreach {
	i => 
	val nr = tr(i).length
	List.range(0,nr) foreach {
	j => 
	val k = math.abs(middle - i) + 2*j
	s(i)(k) = math.max(s(i-1)(k-1), s(i-1)(k+1)) + tr(i)(j)
	}
	println("i = " + s(i).toList)
	}
	s(nrows-1)(0) = s(nrows-2)(1) + tr.last(0)
	s(nrows-1)(2*nrows - 2) = s(nrows-2)(2*nrows-3) + tr.last.last
	List.range(1,nrows-1) foreach {
	j => 
	s(nrows-1)(2*j) = math.max(s(nrows-2)(2*j-1), s(nrows-2)(2*j+1)) + tr.last(j)
	}	
	println("last = " + s(nrows-1).toList)
	s(nrows-1).max
}
