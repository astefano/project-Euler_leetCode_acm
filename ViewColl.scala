case class ViewList[A](self: List[A]) {
  val lv = self.view
  def lazymap[B](f: A => B) = lv.map(f)
  def lazyfilter(p: A => Boolean) = lv.filter(p)
  def lazytake(n: Int) = lv.take(n)
}

object TestViews {

  def main(args: Array[String]) {

    val longL = (1 to 100000).toList

    val l = ViewList(longL)

    val lm = l.lazymap(x => x*2)
    //print return SeqViewM
    println(lm)
    println(lm(0))
    val lm5 = l.lazytake(5)
    println(lm5)
    println(lm5.length)
    println(lm5.reduce(_+_))
  }

}
  
