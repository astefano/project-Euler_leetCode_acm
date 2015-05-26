object P173 {

 def f(l:Int) = l*l+math.round(math.sqrt(l)/2) - List.range(math.round(math.sqrt(l)/2),l+1).map{k=> math.round(math.sqrt(k*k-l/4)) + math.round(math.sqrt((2*k+1)*(2*k+1)-l)-1)/2}.reduceLeft(_+_)

 def fnaive(l: Int) = {
   var n = 1L
   var kM = 1
   val ks = List.range(2, l)
   ks foreach {
     k => 
       val is = List.range(0, k)
       is foreach {
	 i => 
	   if ((2*k+1)*(2*k+1)-(2*i+1)*(2*i+1) <= l) {
	     if (kM < k) kM = k
	     //println("odd: (k, i) =" + (k, i))
	     n+=1 
	   }
	   if (i > 0 && (2*k)*(2*k)-(2*i)*(2*i) <= l) { 
	     if (kM < k) kM = k
	     //println("even: (k, i) =" + (k, i))
	     n+=1 
	   }
       }
   }
   //println("kM = " + kM)
   n
 }

 def gO(k: Int, i: Long) = (2*k+1)*(2*k+1) - (2*i+1)*(2*i+1)

 def gE(k: Int, i: Long) = 2*k*2*k - 2*i*2*i

 import scala.math._

 def g(l: Int) = {
   List.range(2, l/8+1).map{k => 
			val v = ((2L*k+1)*(2*k+1)) - l
			val diffF = if (v >= 1) (math.sqrt(v) - 1)/2 else 0L
			val diff0 =round(diffF)
			val diff = if (diffF > diff0) diff0 + 1L else diff0
			if (k < diff) println("something strange: k = " + k + " diff = " + diff)
			(k - diff)
			  }.foldLeft(0L)(_+_) + 
  (2 to (l/4+1)/2+1).map{k => 
			val v = (2L*k)*(2*k) - l	      
			val diffF = if (v >= 0) math.sqrt(v)/2 else 1L
			val diff0 =round(diffF)
			val diff = if (diffF > diff0) diff0 + 1L else diff0
			(k - diff)
		       }.foldLeft(0L)(_+_)
 }

  def main(args : Array[String]) {
    val l = args(0).toInt
    //println("res = " + f(n))
    //println("res = " + fnaive(n))
    println("res = " + g(l))
    
  }
}
