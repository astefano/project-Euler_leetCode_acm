object P51 {

  def isPrime(n: Int) = n > 1 && !List.range(2, math.floor(math.sqrt(n)).toInt+1).exists(i => (n%i)==0)

  def p51(p: Int) = {
    val ps = p.toString
    val n = ps.length
    var found = false
    var notpr = 0
    var i = 1
    var j = 1
    var d = 1
    //we look for i and j s.t. p[0..i-1]dp[i+1..j-1]dp[j+1..n-1] is prime
    //it cannot be that j is n-1 because there are only 1,3,7,9 possible endings for a prime and we need 8!
    //we first check if i can be 0 (in this case, d > 0)
    while(!found && j < n - 1) {
      while(notpr < 2 && d < 10) {
	val res = d + ps.tail.take(j-1) + d + ps.drop(j+1)
	if (!isPrime(res.toInt)) notpr += 1
	d += 1
      }
      if (notpr < 2) found = true
      j += 1
    }
    if (found) found
    else {
      while(!found && i < n-1) {
	j = i + 1
	notpr = 0
	while(!found && j < n-1) {	  
	  d = 0
	  while(notpr < 3 && d < 10) {	    
	    val res = ps.take(i) + d + ps.drop(i).take(j-i-1) + d + ps.drop(j+1)	    
	    if (i == 1 && j == 3) println(res)
	    if (!isPrime(res.toInt)) {
	      notpr += 1
	      //println(i + " " + j + " " + res)
	    }
	    d += 1
	  }	    
	  if (notpr < 3) found = true
	  j += 1
	}	
	i += 1
      }
      found
    }
  }

  def p513(p: Int) = {
    val ps = p.toString
    val n = ps.length
    var found = false
    var notpr = 0
    var i = 1
    var j = 1
    var k = 2
    var d = 1
    var sols = Set[String]()
    //we look for i and j s.t. p[0..i-1]dp[i+1..j-1]dp[j+1..n-1] is prime
    //it cannot be that j is n-1 because there are only 1,3,7,9 possible endings for a prime and we need 8!
    //we first check if i can be 0 (in this case, d > 0)
    while(!found && j < n - 1) {
      k = j + 1
      while(!found && k < n - 1) {
	notpr = 0
	sols = Set[String]()
	d = 1
	while(notpr < 2 && d < 10) {
	  val res = d + ps.tail.take(j-1) + d + ps.drop(j+1).take(k-j-1) + d + ps.drop(k+1)
	  sols = sols + res
	  if (!isPrime(res.toInt)) notpr += 1 //else println("j = " + j + " k = " + k + " res = " + res)
	  d += 1
	}
	if (notpr < 2) found = true
	k += 1
      }
      j += 1
    }
    if (found) {
      println(notpr + " " + j + " " + k  + " sols1 = " + sols)
      found
    }
    else {
      while(!found && i < n-1) {
	j = i + 1
	notpr = 0
	while(!found && j < n-1) {	  
	  k = j + 1
	  while(!found && k < n-1) {	  
	    d = 0
	    notpr = 0
	    sols = Set[String]()
	    while(notpr < 3 && d < 10) {	    
	      val res = ps.take(i) + d + ps.drop(i).take(j-i-1) + d + ps.drop(j+1).take(k-j-1) + d + ps.drop(k+1)	    
	      sols = sols + res
	      if (!isPrime(res.toInt)) {
	      notpr += 1
	      //println(i + " " + j + " " + res)
	      }
	      d += 1
	    }	    
	    if (notpr < 3) found = true
	    k += 1
	  }
	  j += 1
	}	
	i += 1
      }
      if (found) println("sols = " + sols)
      found
    }
  }

  def p514(p: Int) = {
    val ps = p.toString
    val n = ps.length
    var found = false
    var notpr = 0
    var i = 1
    var j = 1
    var k = 2
    var l = 3
    var d = 1
    var sols = Set[String]()
    //we look for i and j s.t. p[0..i-1]dp[i+1..j-1]dp[j+1..n-1] is prime
    //it cannot be that j is n-1 because there are only 1,3,7,9 possible endings for a prime and we need 8!
    //we first check if i can be 0 (in this case, d > 0)
    while(!found && j < n - 1) {
      k = j + 1
      while(!found && k < n - 1) {
	l = k + 1
	while(!found && l < n - 1) {
	  notpr = 0
	  sols = Set[String]()
	  d = 1
	  while(notpr < 2 && d < 10) {
	    val res = d + ps.tail.take(j-1) + d + ps.drop(j+1).take(k-j-1) + d + ps.drop(k+1).take(l-k-1) + d + ps.drop(l+1)
	    sols = sols + res
	    if (!isPrime(res.toInt)) notpr += 1 else println("j = " + j + " k = " + k + " res = " + res)
	    d += 1
	  }
	  if (notpr < 2) found = true
	  l += 1
	}
	k += 1
      }
      j += 1
    }
    if (found) {
      println(notpr + " " + j + " " + k  + " " + l + " sols1 = " + sols)
      found
    }
    else {
      while(!found && i < n-1) {
	j = i + 1
	notpr = 0
	while(!found && j < n-1) {	  
	  k = j + 1
	  while(!found && k < n-1) {	  
	    l = k + 1
	    while(!found && l < n-1) {	  
	      d = 0
	      notpr = 0
	      sols = Set[String]()
	      while(notpr < 3 && d < 10) {	    
		val res = ps.take(i) + d + ps.drop(i).take(j-i-1) + d + ps.drop(j+1).take(k-j-1) + d + ps.drop(k+1).take(l-k-1) + d + ps.drop(l+1)	    
		println(i + " " + j + " " + k + "\n" + res + "\n" + p)
		sols = sols + res
		if (!isPrime(res.toInt)) {
		  notpr += 1
		  println(i + " " + j + " " + res)
		}
		d += 1
	      }	    
	      if (notpr < 3) found = true
	      l += 1
	    }
	    k += 1
	  }
	  j += 1
	}	
	i += 1
      }
      if (found) println("sols = " + sols)
      found
    }
  }

import scalax.io._
import scalax.file.{ FileOps, Path, NotFileException }

  def main(args: Array[String]) {
    if (args.length > 0) {
      val n = args(0).toInt
      println(p514(n))
    }
    else {
    val start = System.currentTimeMillis
/**/
    val file = "/media/ubuntuPart2/docs/topcoder/primesSmallerThan10to7.txt"
    val rf: Input = Resource.fromFile(file)
    val lines = rf.lines().filter(_!="")    
    lines foreach {
      pli => 
      val cprimes = pli.trim.split(" ").filter(_!="").map(_.toInt)
      cprimes foreach {
	p => 
	  val res = p513(p)
	  if (res) println(p) 
      }
    }

    println( "Time (s): " + ( System.currentTimeMillis() - start ) / 1000. )
    }
  }


}

