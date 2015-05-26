object P74 {

  val facts = Map(0 -> 1, 1 -> 1, 2 -> 2, 3 -> 6, 4 -> 24, 5 -> 120, 6 -> 720, 7 -> 5040, 8 -> 40320, 9 -> 362880)

  def sf(n: BigInt) = n.toString.toList.foldLeft(BigInt(0))((r,c) => r + facts(c - '0'))

  def lenChain(n: BigInt) = {
    var l = 1
    var visited = Set[BigInt](n)
    var newv = sf(n)
    while(!visited(newv) && l < 62) {
      println(newv)
      visited = visited + newv
      newv = sf(newv)
      l += 1
    }
    l 
  }

List.range(1,1000000)

}
