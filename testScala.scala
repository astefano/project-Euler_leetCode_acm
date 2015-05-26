object TestScala {

def merge(a: Array[Int], b: Array[Int]) = {
    val nat = a.length
    val nb = b.length
    var cal = nat - nb
    for(i <- (nb - 1 to 0 by -1)) {
    	  (cal - 1 to 0 by -1).takeWhile{
	  j => 
		  println(cal + " - " + (i, j)+ ":" + (b(i),a(j)))
 	      	  if (b(i) < a(j)) a(j+1)=a(j)
	  	  else a(j+1)=b(i)
		  println(a.toList)
		  if (cal < nat - 1) cal += 1 
		  (b(i) < a(j))
    	       }
	  }
    }

def main(args: Array[String]) {
    val a = Array(1,5,9,0,0)
    val b = Array(2,6)
    merge(a, b)
    println("a = " + a.toList)
    }   

}