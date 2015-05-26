
var ni = math.pow(2, 18).toLong*28433L

var i = 18

while(i <7830457) {
      ni = (2*ni).toString.takeRight(10).dropWhile(c => c == '0').toLong
      //println(ni)
      i += 1
     }
