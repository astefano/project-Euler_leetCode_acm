//m^n = q^p iff both m^n and q^p have the same decomp in prime factors 

//val p100 = primes.takeWhile(_<100)

val p100 = List(2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97)

def multiplicity(n: Int, p: Int): Int = {
     def aux(n: Int, m: Int): Int = if (n % p != 0) m else aux(n/p, m+1)
     aux(n, 0)
     }

def factor(n: Int) = p100.filter{p => (n % p == 0)}.map{p => (p, multiplicity(n, p))}

val ini = List.range(2,101).map(factor(_))

var s = Set[List[(Int,Int)]]()

List.range(2,101) foreach {
      b => 
      s = s ++ ini.map{x => x.map{f => (f._1, f._2*b)}}
      }

s.size
