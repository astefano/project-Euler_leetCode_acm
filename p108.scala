object P108 {

def diophSols(n: Int) = List.range(n+1,2*n+1).filter(x => (1L*n*x) % (1L*x-n) == 0L).length

def main(args: Array[String]) {
  val nstart = args(0).toInt
  //println(diophSols(360))
  var ns = 0

  val res = Stream.from(4).dropWhile{
    n => 
      ns = diophSols(n) 
      println("diophs(" + n + ") = " + ns)
      ns <= 1000
      
  }
  println("res = " + res.head + ns)
  }
}
