object P170 {

  def main(args: Array[String]) {

  val rx = List.range(2, 100)
  val digits = ('0' to '9').toSet

  var m = 9846371502L
  var xf = 0
  var yf = 0
  var zf = 0
  var wf = 0    
  var tf = 0    

  List.range(2, 100) foreach {
    x => 
    List.range(49,100000).filter{e => 
      val ex = (x*e).toString
      (e.toString.toSet & x.toString.toSet).isEmpty && ex.startsWith("98") && ex.toList.distinct == ex.toList && e.toString.toList == e.toString.toList.distinct
			       } foreach {
      y => 
	val xy = (x*y).toString
	val rem = (digits -- x.toString.toSet) -- y.toString.toSet
	val remp = (digits -- xy.toSet)
	val remSubsets = rem.subsets.toList.tail 
	remSubsets foreach { 
	  cands => 
	    cands.toList.permutations.filter{_.head != '0'}.map{p => p.mkString.toInt} foreach {
	      z => 
		val xz = (x*z).toString.toList
		val xyz = (xy + xz.mkString)

//		if (z == 20639) println("(x,y,z) = " + (x,y,z) + "xy = " + xy + " xz = " + xz + " xyz = " + xyz + " remp = " + remp + " m = " +m.toString.take(xyz.length) )

		val c2 = (xz.toSet subsetOf remp)
		if (xz.distinct == xz && c2 && xyz >= m.toString.take(xyz.length)) {
		  if (xyz.length == 10 && z.toString.toSet == rem) {
		    xf = x
		    yf = y
		    zf = z
		    m = xyz.toLong
		    println("(x,y,z) = " + (x,y,z) + "xy = " + xy + " xz = " + xz + " xyz = " + xyz + " remp = " + remp + " rem = " + rem + " m = " +m)
		  }
		  val rem2 = rem -- z.toString.toSet
		  val remp2 = remp -- xz.toSet 
		  rem2.subsets.toList.tail foreach { 
		    cands => 
		      cands.toList.permutations.filter{_.head != '0'}.map{p => p.mkString.toInt} foreach {
			w => 
			  val xw = (x*w).toString.toList
			  val xyzw = (xy + xz.mkString + xw.mkString)
			  if (xw.distinct == xw && (xw.toSet subsetOf remp2) && xyzw >= m.toString.take(xyzw.length)) {
			    if (xyzw.length == 10 && w.toString.toSet == rem2) {
			      xf = x
			      yf = y
			      zf = z
			      wf = w
			      m = xyzw.toLong
			      println("(x,y,z,w) = " + (x,y,z,w) + "xy = " + xy + " xz = " + xz + " xw = " + xw + " xyzw = " + xyzw + " remp2 = " + remp2 + " rem2 = " + rem2 + " m = " +m)
			    }
			    val rem3 = rem2 -- w.toString.toSet
			    val remp3 = remp2 -- xw.toSet 
			    rem3.subsets.toList.tail foreach { 
			      cands => 
				cands.toList.permutations.filter{_.head != '0'}.map{p => p.mkString.toInt} foreach {
				  t => 
				    val xt = (x*t).toString.toList
				    val xyzwt = (xy + xz.mkString + xw.mkString + xt.mkString )
				    if (xt.distinct == xt && (xt.toSet subsetOf remp3) && xyzwt >= m.toString.take(xyzwt.length)) {
				      if (xyzwt.length == 10 && t.toString.toSet == rem3) {
					xf = x
					yf = y
					zf = z
					wf = w
					tf = t
					m = xyzwt.toLong
					println("(x,y,z,w,t) = " + (x,y,z,w,t) + "xy = " + xy + " xz = " + xz + " xw = " + xw + " xt = " + xt + " xyzwt = " + xyzwt + " remp3 = " + remp3 + " rem3 = " + rem3 + " m = " +m)
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

  println("(x,y,z,w,t) = " + (xf, yf, zf,wf,tf) + " prod = " + ((xf*yf).toString + (xf*zf).toString + (xf*wf).toString + (xf*tf).toString) + " alld = " + xf.toString + yf.toString + zf.toString + wf.toString + tf.toString)

  }
}
