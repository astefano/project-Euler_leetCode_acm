def ackermann(m : Int, n : Int) : Int = {
  if (m == 0) n + 1
  else if (n == 0) ackermann(m - 1, 1)
  else ackermann(m - 1, ackermann(m, n - 1))
}

import scala.collection.mmutable._

def ackermannIt(m : Int, n : Int) = {

val s = new Stack[Int]
var count = 0
s.push(m)
s.push(n)

while (s.size > 1) {
  val nc = s.pop
  val mc = s.pop
  if (mc == 0) s.push(nc + 1)
  else if (nc == 0) {
    s.push(mc - 1)
    s.push(1)
  }
  else {
    s.push(mc - 1)
    s.push(mc)
    s.push(nc - 1)
    count = count + 1
  }
}

println("count = " + count)
s.pop
}

for (m <- List.range(2, 3); n <- List(m+1, 10); if (ackermann(m, n) != ackermannIt(m, n))) yield (m, n) 
