object LazyList extends App {
  def empty[A]: LazyList[A] = new LazyList[A] {
    lazy val uncons = None
  }

  def cons[A](h: => A, t: => LazyList[A]) : LazyList[A] = new LazyList[A] {
    lazy val uncons = Some( (h,() => t) )
  }

  def from(s: Int): LazyList[Int] = new LazyList[Int] {
    lazy val uncons = Some( (s,() => from(s + 1)) )
  }

  println("Hallo2")

}

trait LazyList[A] {
  import LazyList._

  def uncons: Option[(A,() => LazyList[A])]

  def fmap[B](f: A => B): LazyList[B] = uncons match {
    case None          => empty
    case Some( (h,t) ) => cons(f(h),t().fmap(f))
  }

  def take(i: Int): LazyList[A] = uncons match {
    case None          => empty
    case Some( (h,t) ) => if (i <= 0) empty else cons(h,t().take(i - 1))
  }

  override def toString: String = uncons match {
    case None          => "[]"
    case Some( (h,t) ) => "[" ++ h.toString ++ ",..]"
  }
}

/*
def inits: LazyList[LazyList[A]] = uncons match {
    case None          => empty
    case Some( (h,t) ) => cons(empty,t().inits.fmap(cons(h,_)))
  }
*/
  
