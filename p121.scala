def p(k: Int, t: Int) = if (t == 0) (1, k+1) else (k, k+1)

def gen(n: Int) = (for(nr <- List.range(0, (if (n%2 == 0) n/2 else n/2+1))) yield (List.fill(nr)(1) ::: List.fill(n - nr)(0)).permutations.toList).flatten

def pl(l: List[Int]) = l.zipWithIndex.map{el => p(el._2+1, el._1)}.foldLeft((1L,1L))((r,c) => (r._1*c._1, r._2*c._2))

def sum(n: Int) = gen(n).map{l => pl(l)}.foldLeft((0L,0L))((r, c) => (r._1 + c._1, c._2))

sum(15)
