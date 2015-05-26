/* the spiral of length n has a diag of (2n-1) elems and the corners: s(0,n) = (n-1)^2 + n, s(0,0) = (n-1)^2 + 2n, s(n,0) = (n-1)^2 + 3n, s(n,n) = (n-1)^2 + 4n, i.e., (n+1)^2.
 */ 

var n = 7
var nd = 13
var np = 8

while(1.*np/nd > 0.1) {
      val x = 1L*n*n
      np += List(x+n+1, x+2*n+2, x+3*n+3).filter(isPrime(_)).length
      nd += 4
      n += 2
      }
