object P26 {

  // dec expansion of a/b; we have a boundLen for those fractions which have infinite expansion
  def decExp(a: Int, b: Int) = {
    val p = b.toString.length - a.toString.length + 1
    val pow10 = math.pow(10, p).toInt
    var rem = a * pow10
    var dec = List.range(1, p).map(x => 0)
    var rems = Set[Int]()
    var i = 0
    val boundLen = 100
    while (!rems(rem) && rem != 0 && i < boundLen){      
      rems = rems + rem
      dec = dec :+ (rem / b)
      println("rem = " + rem + " dec = " + dec)
      rem = (rem % b)*10
      i += 1
    } 
    dec
  }


  // length of the period in the dec expansion of a/b 
  def lengthDecExp(a: Int, b: Int) = {
    val p = b.toString.length - a.toString.length + 1
    val pow10 = math.pow(10, p).toInt
    var rem = a * pow10
    //the numbers of starting zeros after comma
    var i = p - 1
    var rems = Map[Int, Int]().empty
    val boundLen = 100000
    while (!rems.keys.toSet(rem) && rem != 0 && i < boundLen){
      rems = rems + (rem -> i)
      rem = (rem % b)*10
      i += 1
      //println("i = " + i + "\nrems = " + rems)
    } 
    //if we stop with rem = 0 then there's no period
    if(rem != 0) (i - rems(rem)) else 0
  }

lengthDecExp(1,6)

def main(args: Array[String]) {
    val start = System.currentTimeMillis
    val lengths = List.range(2, 1000).map(i => (i, lengthDecExp(1,i)))
    println(lengths)
    val res = lengths.max
    println("res = " + res)
    
    println( "Time (s): " + ( System.currentTimeMillis() - start ) / 1000. )
  }
}  



