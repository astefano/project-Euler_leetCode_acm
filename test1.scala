object Snake {

def issquare(n: Int) = {
  val aux = math.floor(math.sqrt(n))
  aux * aux == n
}

  def f(l: List[Int], r: List[Int]) : Set[List[Int]] = {
  if (l == List()) Set(r)
  else {
    for(y <- gets(l.head); z <- f(l.tail, y +: r); if (!z.toSet(y))) yield y +: z
  }
}

val c = (1 to 25).toSet - 20 - 13

def gets(v: Int) = c.filter(x=>issquare(x + v))

//def isValid(l: List[Int]) = 

def main(args: Array[String]) {

  var all = List[Int]()

  Set(5, 16) foreach {
    x4 => 
      all :+= x4
    Set(5, 16) -- all.toSet foreach {
      x5 => 
	all :+= x5
      Set(12, 3, 23) foreach {
	x18 => 
	  all :+= x18
	Set(12, 3, 23) - x18 foreach {
	  x19 => 
	    all :+= x19
	    val aux = gets(x4) -- all.toSet 
	    if (aux == Set()) all = all 
	    else aux foreach {
	    x3 => 
	      all :+= x3
	    val aux = gets(x3) -- all.toSet 
	    if (aux == Set()) all = all 
	    else aux foreach {
	      x2 => 
		all :+= x2
		val aux = gets(x2) -- all.toSet 
		if (aux == Set()) all = all
		else aux foreach {
		x1 => 
		  all :+= x1
		  val aux = gets(x5) -- all.toSet 
		  if (aux == Set()) all = all 
		  else aux foreach {
		  x6 => 
		    all :+= x6
		  val aux = gets(x6) -- all.toSet 
		  if (aux == Set()) all = all 
		  else aux foreach {
		    x7 => 
		      all :+= x7
		    val aux = gets(x7) -- all.toSet
		    if (aux == Set()) all = all 
		    else aux foreach {
		      x8 => 
			all :+= x8
		      val aux = gets(x8) -- all.toSet 
		      if (aux == Set()) all = all 
		      else aux foreach {
			x9 => 
			  all :+= x9
			val aux = gets(x9) -- all.toSet 
			if (aux == Set()) all = all 
			else aux foreach {
			  x10 => 
			    all :+= x10
			  val aux = gets(x10) -- all.toSet 
			  if (aux == Set()) all = all 
			  else aux foreach {
			    x11 => 
			      all :+= x11
			    val aux = gets(x11) -- all.toSet 
			    if (aux == Set()) all = all 
			    else aux foreach {
			      x12 => 
				all :+= x12
			      val aux = gets(x12) -- all.toSet 
			      if (aux == Set()) all = all 
			      else aux foreach {
				x13 => 
				  all :+= x13
				val aux = gets(x13) -- all.toSet
				if (aux == Set()) all = all else aux foreach {
				  x14 => 
				    all :+= x14
				  val aux = gets(x14) -- all.toSet
				  if (aux == Set()) all = all else aux foreach {
				    x15 => 
				      all :+= x15
				    val aux = gets(x15) -- all.toSet
				    if (aux == Set()) all = all else aux foreach {
				      x16 => 
					all :+= x16 
				      val aux = gets(x16) -- all.toSet
				      if (aux == Set()) all = all else aux foreach {
					x17 => 
					  all :+= x17
					if (gets(x17)(x18)) {
					  val aux = gets(x19) -- all.toSet
					  if (aux == Set()) all = all else aux foreach {
					    x20 =>
					      all :+= x20
					    val aux = gets(x20) -- all.toSet
					    if (aux == Set()) all = all else aux foreach {
					      x21 =>
						all :+= x21
					      val aux = gets(x21) -- all.toSet
					      if (aux == Set()) all = all else aux foreach {
						x22 =>
						  all :+= x22
						val x23 = ((c -- all.toSet) & gets(x22))
						  if (x23 != Set() && all.size > 20)
						    println("all = " + all + " x23 = " + x23)
					      }
					    }
					  }
					}
				      }
				    }
				  }
				}
			      }
			    }
			  }
			}
		      }
		    }
		  }
		  }
		}
	    }
	    }
	}
      }
    }
  }
}

}
