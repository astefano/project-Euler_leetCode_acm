def lastD(n : Int) = {
var d10 : Long = n
var i = 1
while(i < n) {
  d10 = (d10*n).toString.takeRight(10).toLong
  //println(d10)
  i += 1
}
d10
}

List.range(1,1000).foldLeft(0L)(_.toString.takeRight(10).toLong+lastD(_))


