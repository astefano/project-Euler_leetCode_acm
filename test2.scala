object Tangram {

def main(args: Array[String]) {

//  val x1 = 4
//  val x2 =  9

  val r = for(
    x1 <- (0 to 4);
    x2 <- (0 to 9).toSet - x1;
    x3 <- (0 to 9).toSet - x1 - x2; 
    x7 <- (0 to 9).toSet - x1 - x2 - x3; 
/*    x6 <- (x7 - 4 to 9).toSet - x1 - x2 - x3 - x7; 
    x8 <- (x7 - 3 to 9).toSet - x1 - x2 - x3 - x6 - x7; 
    x9 <- (x7 - 3 to 9).toSet - x1 - x2 - x3 - x6 - x7 - x8;
    x4 <- (0 to 3).toSet - x1 - x2 - x6 - x8 - x9;
    */ 
    x6 <- (0 to 9).toSet - x1 - x2 - x3 - x7; 
    x8 <- (0 to 9).toSet - x1 - x2 - x3 - x6 - x7; 
    x9 <- (0 to 9).toSet - x1 - x2 - x3 - x6 - x7 - x8;
    x4 <- (0 to 9).toSet - x1 - x2 - x3 - x6 - x7 - x8 - x9;
    x5 <- (0 to 9).toSet - x1 - x2 - x3 - x4 - x6 - x7 - x8 - x9;
    x10 <- (0 to 9).toSet - x1 - x2 - x3 - x4 - x5 - x6 - x7 - x8 - x9;
    val d12 = math.abs(x1-x2);
    val d16 = math.abs(x1-x6);
    val d34 = math.abs(x3-x4);
    val d45 = math.abs(x4-x5);
    val d27 = math.abs(x2-x7); 
    val d38 = math.abs(x3-x8); 
    val d23 = math.abs(x2-x3); 
    val d36 = math.abs(x3-x6); 
    val d67 = math.abs(x7-x6); 
    val d910 = math.abs(x9-x10); 
    val d78 = math.abs(x7-x8); 
    val d79 = math.abs(x7-x9); 
    val d89 = math.abs(x8-x9); 
    val d610 = math.abs(x6-x10); 
    val d510 = math.abs(x5-x10);
    val d56 = math.abs(x5 - x6);
    val d49 = math.abs(x4-x9);
    val d69 = math.abs(x6-x9);
    val ld = List(d12, d16, d34, d45, d27, d38, d23, d67, d910, d78, d79, d89, d610, d510, d56);
    val t1 = List(d16, d12, d27, d67);
    val t2 = List(d27, d23, d38, d78);
    val t3 = List(d49, d34, d38, d89);
    val t4 = List(d49, d45, d510, d910);
    val t5 = List(d510, d610, d56);
    val t6 = List(d79, d910, d610, d67);
    val t7 = List(d89, d79, d78);
    if (t1.max <= 5 && t1.filter(_==5).length==1 && 
	t2.max <= 4 && t1.filter(_==4).length==1 && 
	t3.max <= 5 && t3.filter(_==5).length==1 && 
	t4.max <= 5 && t4.filter(_==5).length==1 && 
	t5.max <= 3 && t5.filter(_==3).length==1 && 
	t6.max <= 4 && t6.filter(_==4).length==1 && 
	t7.max <= 3 && t7.filter(_==3).length==1 
      )
  ) yield (List(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10), ld)

  println("r = " + r.foldLeft("")(_+"\n"+_))
}

}

//(List(0, 2, 6, 9, 8, 5, 1, 3, 4, 7),List(2, 5, 3, 1, 1, 3, 4, 4, 3, 2, 3, 1, 2, 1, 3))
//(List(0, 3, 6, 9, 8, 5, 1, 2, 4, 7),List(3, 5, 3, 1, 2, 4, 3, 4, 3, 1, 3, 2, 2, 1, 3))
