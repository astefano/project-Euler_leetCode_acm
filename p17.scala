object P13{

def mapNumber2Word(n: Int): String = {
     val ns = n.toString
     val l = ns.length
     if (l == 1) {
       if (n == 1) "one"
       else if (n == 2) "two"
       else if (n == 3) "three"
       else if (n == 4) "four"
       else if (n == 5) "five"
       else if (n == 6) "six"
       else if (n == 7) "seven"
       else if (n == 8) "eight"
       else "nine"
     }
    else if (l == 2) {
      if (n == 10) "ten"
      else if (n == 11) "eleven"
      else if (n == 12) "twelve"
      else if (n == 13) "thirteen"
      else if (n == 15) "fifteen"
      else if (n == 18) "eighteen"
      else if (n < 20) mapNumber2Word(n%10)+"teen"
      else if (n == 20) "twenty"
      else if (n == 30) "thirty"
      else if (n == 40) "forty"
      else if (n == 50) "fifty"
      else if (n == 80) "eighty"
      else if (n > 10 && n % 10 == 0) mapNumber2Word(n/10)+"ty"
      else mapNumber2Word(n/10*10) + mapNumber2Word(n%10)
    }
  //l == 3 
  else {
    if (n % 100 == 0) mapNumber2Word(n/100) + "hundred"
    else mapNumber2Word(n/100)+"hundred"+"and"+ mapNumber2Word(n%100)
  }  
}


def main(args: Array[String]) {
  val start = System.currentTimeMillis
  val r= List.range(1,1000).map{mapNumber2Word(_)} :+ "onethousand"
  val res = r.foldLeft(0)(_+_.length)
  println("r = " + r.foldLeft("")(_+"\n"+_) + " res = " + res)
  println( "Time (s): " + ( System.currentTimeMillis() - start ) / 1000. )
  }
} 
