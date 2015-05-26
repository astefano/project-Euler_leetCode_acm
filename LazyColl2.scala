//adopted & adapted from 
abstract class LazyColl[+A] {
  def el : A
  def body : LazyColl[A]
  def mmap[B](f: A => B) : LazyColl[B]
  def mfilter(p: A => Boolean) : LazyColl[A]
  def take(n: Int) : LazyColl[A]
  def count : Int = 0
}

case object LazyEmpty extends LazyColl[Nothing] {
  def el : Nothing = throw new Exception("LazyEmpty has no elem")
  def body : LazyColl[Nothing] = throw new Exception("LazyEmpty has no body")
  //map, filter, take lazyEmpty returns lazyEmpty
  def mmap[B](f: Nothing => B) = LazyEmpty
  def mfilter(p: Nothing => Boolean) = LazyEmpty
  def take(n: Int) = LazyEmpty
}

class LazyCompose[A] (val el : A, _body : => LazyColl[A]) extends LazyColl[A] {
  lazy val body = _body

  def mmap[B](f: A => B) : LazyColl[B] = new LazyCompose(f(el), body.mmap(f))

  def mfilter(p: A => Boolean) : LazyColl[A] = if (p(el)) new LazyCompose(el, body.mfilter(p)) else body.mfilter(p)

  def take(n : Int) = if (n > 0) new LazyCompose(el, body.take(n-1)) else LazyEmpty

  override def count = {
    var r = 1
    var copy_body = _body
    while (copy_body != LazyEmpty) {
      r += 1
      copy_body = copy_body.body
    }
    r
  }
}

trait LazyComposable[A] {
  def compose (body : => LazyColl[A]) : LazyColl[A]
}

object LazyCollImplicit {
  implicit def lazyComposable[A] (el : A) : LazyComposable[A] = new LazyComposable[A] {
    def compose (body : => LazyColl[A]) = new LazyCompose(el, body)
  }  
}

object TestLazyCompo {

  import LazyCollImplicit._ 
  
  def main(args : Array[String]) {
    
    def initFrom(n : Int) : LazyColl[Int] = n compose initFrom(n+1)

    val ml = initFrom(3)
    println(ml.el)
    println(ml.body.el)

    val ml2 = 3 compose (1 compose (2 compose LazyEmpty))
    val ml3 = ml2.mmap (x => 2*x)

    println(ml2.el)
    println(ml3.el)
    
    var ml4 = ml.mfilter(x => x < 10).take(5)
    println("cc = " + ml4.count)
    var c = 0
    while (ml4 != LazyEmpty) {
      println(ml4.el)
      ml4 = ml4.body
      c += 1
    }
    println("c = " + c)        
  }
}
