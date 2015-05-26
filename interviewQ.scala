/* from http://www.techinterview.org/ */

/*http://www.techinterview.org/post/526374214/reverse-a-string/ */

def reverse(a: Array[Int]) = {
  val n = a.length  
  for (i <- (0 to (n-1)/2)) {
    val aux = a(i)
    a(i) = a(n-1-i)
    a(n-1-i) = aux
  }
  a
} 

def reverse(a: Array[Array[Int]]) = {
  val n = a.length  
  for (i <- (0 to (n-1)/2)) {
    val aux = a(i)
    a(i) = a(n-1-i)
    a(n-1-i) = aux
  }
  a
} 

def reverse(a: Array[Char]) = {
  val n = a.length  
  for (i <- (0 to (n-1)/2)) {
    val aux = a(i)
    a(i) = a(n-1-i)
    a(n-1-i) = aux
  }
  println(a.toList)
  var i = 0  
  while(i < n) {
    var j = i
    while(j < n && a(j) != ' ') j += 1  
    val pspace = j
    val len = pspace - i 
    println("p = " + pspace + " j = " + j + " i = " + i + " len = " + len + " a = " + a.toList)
    j = 0
    while(j < len/2) {    
      val aux = a(i+j)
      a(i+j) = a(i+len-j-1)
      a(i+len-j-1) = aux
      println("p = " + pspace + " j = " + j + " i = " + i + " len = " + len + " a = " + a.toList)
      j += 1
    }  
    i = pspace + 1
  }
  a
} 

reverse(Array('t','h','e',' ','s','k','y', ' ', 'i', 's', ' ', 'b', 'l', 'u', 'e'))

/*http://www.techinterview.org/post/526370758/100-doors-in-a-row/ */

def transfK(l: List[Boolean], k: Int) = l.zipWithIndex.map(x => if ((x._2+1) % k == 0) !x._1 else x._1)

def transf(l: List[Boolean]) = (1 to l.length).foldLeft(l)((r,c) => transfK(r, c))

def gen(n: Int) = transf((1 to n).map{_ => false}.toList)

//only perfect squares are true
(4 to 100 by 2).foreach{i => println(gen(i).zipWithIndex.filter(x => x._1 == true).map(_._2 + 1))}



