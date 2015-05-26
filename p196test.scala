
def row(n: Int) = List.range(n*(n-1)/2+1, n*(n-1)/2+n+1)

def getNeighO(x: Int, r: Int) = List(x-2*r+2, x-2*r+4, x-r+1, x, x+r-1, x+r+1, x+2*r, x+2*r+2) 

def check(r: Int) = {
      val rc = row(r)
      val rp1 = row(r-1)
      val rp2 = row(r-2)
      val rn1 = row(r+1)
      val rn2 = row(r+2)
      val n = rc.length
	val res = List.range(1, n-3).filter{
      i => 
      getNeighO(rc(i), r) != List(rp2(i-1), rp2(i+1), rp1(i), rc(i), rn1(i-1), rn1(i+1), rn2(i-1), rn2(i+1))
      }
	res
      }

