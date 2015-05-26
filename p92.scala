object P92 {

  val sq = Map((0->0),(1->1),(2->4),(3->9),(4->16),(5->25),(6->36),(7->49),(8->64),(9->81))

  val sq1 = sq map (x => ((x._1+'0').toChar -> x._2))

  def f(n: Int) = n.toString.foldLeft(0)(_+sq1(_))

  def rew(n: Int): Int = { print(n + " -> "); if (n == 89 || n == 1) n else rew(f(n)) }
  //def rew(n: Int): Int = if (n == 89 || n == 1) n else rew(f(n))

  def p92(lim: Int) = {
    var i = 2
    var t89 = 0      
    while ( i < lim ) {
      if (rew(i) == 89) t89 += 1
      i += 1
    }
    t89
  }

  def main(args: Array[String]) {
    rew(2)
    //println(p92(100000))
  }
}
