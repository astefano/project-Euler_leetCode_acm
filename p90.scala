object P90 {

  val digits = (List.range('0','9') :+ '9').map(_.toString)

  val pp = Set("01","04","09","16","25","36","49","64","81")

  //def eq(c1: List[String], c2: List[String]) =     

  def valid(c01: Set[String], c02: Set[String]) = {    
    var rem = pp
    var c1 = c01
    var c2 = c02
    //if (c1.filter(_=="69").length == 2) c1.filter(_!="69") :+ "6" :+ "9"
    //if (c2.filter(_=="69").length == 2) c2.filter(_!="69") :+ "6" :+ "9"
    if (c01.filter(_=="6").size > 0) c1 = c01 + "9"
    if (c01.filter(_=="9").size > 0) c1 = c01 + "6"
    if (c02.filter(_=="9").size > 0) c2 = c02 + "6"
    if (c02.filter(_=="6").size > 0) c2 = c02 + "9"
    c1.dropWhile {
      f1 => 
	c2.filter(_!=f1).dropWhile {
	  f2 => 
	    val f12 = f1+f2
	    val f21 = f2+f1
	    if (rem(f12)) rem = rem - f12
	    else if (rem(f21)) rem = rem - f21
	    //println(f12 + " " + f21 + rem)
	    (rem != Set())
	}
      (rem != Set())
    }
    val r = (rem == Set())
    //if (r) println(c01.mkString(" ") + ",   " + c02.mkString(" "))
    r
  }

  //valid(List(0,1,2,3,4,5).map(_.toString), List(0,1,2,3,8,6,9).map(_.toString))

  def main(args: Array[String]) {
    val combs = digits.combinations(6).toList
    var n = 0
    val all0 = (for(c1 <- combs; c2 <- combs; if (c1 != c2 && valid(c1.toSet,c2.toSet))) yield ((c1, c2))).toSet
    var all = Set[(List[String], List[String])]()
    all0.foreach {
      el => 
      if (!all((el._2,el._1))) all = all + el
    }
    println("all = \n" + all.mkString("\n") + "\nsize = " +  all.size)
  }

}
