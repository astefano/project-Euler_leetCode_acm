object LazyColl {

  def empty[A] : LazyColl[A] = new LazyColl[A] {
    lazy val decomp = None
  }

  def comp[A](el : => A, body : => LazyColl[A]) : LazyColl[A] = new LazyColl[A] {
    lazy val decomp = Some( (el, () => body) )
  }

}

trait LazyColl[A] {

  import LazyColl._ 

  def decomp : Option[(A, LazyColl[A])]

  def lazymap[B](f: A => B) : LazyColl[B] = decomp match {
    case None => empty
    case Some( (el, body) ) => comp(f(el), body.lazymap(f))
  }

  def init(l: List[A]) : LazyColl[A] = new LazyColl[A] {    
    lazy val decomp = l match {
      case None => empty
      case h::t => Some( (h, () => init(t)) )
    }
  }

} 

object TestLazy extends App {
  lazy val l = LazyColl init List(1,2,3)
  println(l lazymap(x => 2*x))
}

  

/*
def lazymap[A, B](f: A => B, l: => List[A]) = {
  if (l == Nil) Nil
  else {
    lazy val lazyl: Stream[???] = f(l(0)) #:: lazyl.tail.map(f) 
    lazyl
  }
}
*/
