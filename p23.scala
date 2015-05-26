var sall = List.range(1, 28124).toSet

var s = Set[Int]()

var i = 12 

while(i <= 28123/2+1) {
      var j = i
      while(j <= 28123-i) {
      if (sigma(i) > i && sigma(j) > j && !s(i+j)) s = s + (i+j)
      j = j + 1
      }
      i = i + 1 }

(sall -- s).toList.sorted
