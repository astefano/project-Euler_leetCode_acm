object P222 {

  import scala.math._

  val l = 100

  //r is the remaining set of radii, lr is the last used radius, res is the result
  def f(r: Set[Int], lr: Int, res: Double) : Double = {
    println("r = " + r + " lr = " + lr + " res = " + res)
    if (r.size == 1) {
      val rl = r.toList
      val rmin = min(2*sqrt(lr*rl(0)), sqrt(l*(2*(lr+rl(0))-l)))
      //println("adding " + rmin)  
      res + rmin + rl(0) 
    }
    else {
    var rm = 0  
    var rmin = 101.
    r foreach {
      ri => 
	val v = min(2*sqrt(lr*ri), sqrt(l*(2*(lr+ri)-l)))
	if (rmin > v) { 
	  rmin = v	
	  rm = ri
	}
    }
    //println("adding " + rmin)  
    f(r - rm, rm, res + rmin)
    }
  }

  def main(args: Array[String]) {
    val start = System.currentTimeMillis
    
    val radii = (30 to 49).toSet
    //val radii = Set(28, 31, 34, 35, 36, 37, 40, 45, 49)

    val res = f(radii, 50, 50)   

    println("res = " + res)

    println( "Time (s): " + ( System.currentTimeMillis() - start ) / 1000. )
  }
}
