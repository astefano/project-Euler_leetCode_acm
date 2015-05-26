object Interview {
  
  def transf(l: List[Int]) = {
    val ll = l.sorted
    val n = l.size   
    if (n % 2 == 1) ll.view.take(n/2+1).zip(ll.drop(n/2+1)).map(x => List(x._1,x._2)).flatten :+ ll(n/2) else
      ll.view.take(n/2).zip(ll.drop(n/2)).map(x => List(x._1,x._2)).flatten
  }

  transf(List(4,3,2,1,5))

  trait InstantaneousTime {
    val repr: Int
    override def equals(other: Any) : Boolean = other match {
      case that: InstantaneousTime =>
	if(this eq that) {
	  true
	} else {
	  (that.## == this.##) &&
	  (repr == that.repr) && (other.equals(this))
	}
      case _ => false
    }
    override def hashCode() : Int = repr.##
  }

  trait Event extends InstantaneousTime {
    val name: String
    override def equals(other: Any): Boolean = other match {
      case that: Event =>
	if(this eq that) {
	  true
	} else {
	  (repr == that.repr) &&
	  (name == that.name)
	}
      case _ => false
    }
  }

  val x = new InstantaneousTime {
    val repr = 2
  }

  val y = new Event {
    val name = "TestEvent"
    val repr = 2
  }

// this only handles functions with a single argument.
 def  memo[X,R](f: X=>R)={
 // a WeakHashMap will release cache members if memory tightens
     val cache=new scala.collection.mutable.WeakHashMap[X,R];
 {(x:X) => cache.getOrElseUpdate(x,f(x));}
 }
 
  // function to be memoized     
  def f(i:Int)={println("called f with "+i); i*2+1 }
   
  // this needs to be a function; if it were a method (with def)
  // it would point to--- rather than evaluate--- the memo call.
  val g=memo(f);

  def testmemo = {
    for (j <- 1 to 3){
      println(g(j));
    }
    for (j <- 1 to 3){
      // f will no be called by these invocations of g
      println(g(j));
    }
  }

for {
x <- 2 to N
y <- 2 to x
if (x % y == 0)
} yield (x,y)

(2 to N) flatMap (x => (2 to x) withFilter (y => x % y == 0) map (y => (x, y)))

(2 to N) map (x => (2 to x) flatMap (y => if ((x % y) == 0) (x, y)))

  val booleans = for (x <- integers) yield x > 0
  val booleans = integers map { x => x > 0 }
  val booleans = new Generator[Boolean] {
    def generate = (x: Int => x > 0)(integers.generate)
  }
  val booleans = new Generator[Boolean] {
    def generate = integers.generate > 0
  }

//a matrix a[3,3] encoded as l1,...,l9
  def isValidSq15(l: List[Int]) = {
    //sum on diag 
    if( l(0) + l(4) + l(8) != 15) false else {
      //sum of rows 
      val r = (0 to 2).dropWhile(i => l(3*i) + l(3*i+1) + l(3*i+2) == 15)
      if (r.isEmpty) {
	//sum columns
	val c = List(0,3,6).dropWhile(i => l(i) + l(i+3) + l(i+6) == 15)
	if (c.isEmpty) {
	  println(s"$l(0) $l(1) $l(2)\n$l(3) $l(4) $l(5)\n$l(6) $l(7) $l(8)")
	  true 
	}
	else false
      }
	else false
    }

  def genSq15() = {
    val l = (1 to 9)
    l.permutations.filter(lp => isValidSq15(lp))
  }



    def main(args: Array[String]) { 
      //testmemo
      //genSq15
    }

}


