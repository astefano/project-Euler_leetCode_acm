def quicksort(s : Array[Int], l : Int, r : Int) : Array[Int] = {
     if (l >= r) s
     else {
       val i = List.range(0, s.length - 1).indexWhere(p => s(p) > s(p + 1))
       if (i > -1) {
	 val aux = s(i)
	 s(i) = s(i + 1)
	 s(i + 1) = aux
	 quicksort(s, l, i - 1)
	 quicksort(s, i + 1, r)
       }
       else s
     }
}


val a = Array[Int](6, 5, 3, 2)

quicksort(a, 0, a.length)

import scala.collection.mmutable._

def quicksortIter(a: Array[Int], l : Int, r : Int) = {
val s = new Stack[Int]
s.push(l)
s.push(r)
while (s.size > 0) {
  val cr = s.pop
  val cl = s.pop
  val i = List.range(0, a.length - 1).indexWhere(p => a(p) > a(p + 1))
  if (i > -1) {
    val aux = a(i)
    a(i) = a(i + 1)
    a(i + 1) = aux
    s.push(l)
    s.push(i - 1)
    s.push(i + 1)
    s.push(r)
  }
}
a
}

val a = Array[Int](6, 5, 3, 2)

quicksortIter(a, 0, a.length)
