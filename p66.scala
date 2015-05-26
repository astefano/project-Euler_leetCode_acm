object P66 {

def findx(d: Int, limy: BigInt) = {
  var x = 0
  var y = 1
  var found = false
  while(!found && y < limy) {
    val r = BigInt(y)*y*d + 1
    val sd = math.sqrt(d).toInt
    var res = List.range(sd*y, d*y).dropWhile(x => BigInt(x)*x != r)
    if (res != List()) {
      found = true
      x = res(0)
    }
    y += 1
  }
  x
}

def main(args: Array[String]) {
/*from Mathematica
 * Max[Reap[For[n = 2, n <= 1000, n++, 
    If[! IntegerQ[Sqrt[n]], Sow[x /. ToRules[sol[n]]]]]][[2, 1]]]
 */
//should have solved it with the length of the continued fractions for sqrt(d)
16421658242965910275055840472270471049

}


