object P89 {

  val rtoN = Map(('I' -> 1), ('V' -> 5), ('X' -> 10), ('L' -> 50), ('C' -> 100), ('D' -> 500), ('M' -> 1000))

  def getN(s: String) = { 
    def aux(s: List[Char], v: Int): Int = s match {
      case List() => v
      case List(c) => rtoN(c) + v
      case h1::h2::t => {
	val vh1 = rtoN(h1)
	val vh2 = rtoN(h2)
	if (vh1 < vh2) aux(t, v + vh2 - vh1) else aux(h2::t, v + vh1)
      }
    }
    aux(s.toList, 0)
  }

  //n < 5000
  def getR(n: Int): String = { 
    val thousands = n / 1000
    var sn = List.range(0,thousands).map(x => 'M').mkString
    val hundreds = (n % 1000) / 100   
    if (hundreds == 4) sn += "CD"
    else if (hundreds < 4) sn += List.range(0,hundreds).map(x => 'C').mkString
    else if (hundreds == 9) sn += "CM"
    else sn += "D" + List.range(0,hundreds-5).map(x => 'C').mkString
    val tens = (n % 100) / 10   
    if (tens == 4) sn += "XL"
    else if (tens < 4) sn += List.range(0, tens).map(x => 'X').mkString
    else if (tens == 9) sn += "XC"
    else sn += "L" + List.range(0,tens-5).map(x => 'X').mkString
    val ones = (n % 10)    
    if (ones == 4) sn += "IV"
    else if (ones < 4) sn += List.range(0, ones).map(x => 'I').mkString
    else if (ones == 9) sn += "IX"
    else sn += "V" + List.range(0,ones-5).map(x => 'I').mkString    
    sn
  }

  //def minimise(s: String) = 

  import scalax.io._
  import scalax.file.{ FileOps, Path, NotFileException }


  def main(args: Array[String]) {
  val start = System.currentTimeMillis
  val file = "/media/ubuntuPart2/docs/topcoder/romans.txt"
  val rf: Input = Resource.fromFile(file)

  val lines = rf.lines().filter(_!="")
  val nlines = lines.size
  println("n lines = " + nlines)
  var saved = 0
  lines foreach {
      l => 
	val word = l.trim
        saved += word.length - getR(getN(word)).length
  }
  println("saved ch = " + saved)
  println( "Time (s): " + ( System.currentTimeMillis() - start ) / 1000. )
  }
}
