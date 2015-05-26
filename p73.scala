/*Consider the fraction, n/d, where n and d are positive integers. If nd and HCF(n,d)=1, it is called a reduced proper fraction.

If we list the set of reduced proper fractions for d  8 in ascending order of size, we get:

1/8, 1/7, 1/6, 1/5, 1/4, 2/7, 1/3, 3/8, 2/5, 3/7, 1/2, 4/7, 3/5, 5/8, 2/3, 5/7, 3/4, 4/5, 5/6, 6/7, 7/8

It can be seen that there are 3 fractions between 1/3 and 1/2.

How many fractions lie between 1/3 and 1/2 in the sorted set of reduced proper fractions for d  12,000?


 */ 
def countfarey(n : Int, asc : Boolean ) = {
// Adapted from """Python function to print the nth Farey sequence, either ascending or descending."""
	var i = 0 
        var a = 0 
        var b = 1
        var c = 1
        var d = n         
	var count = 0
	var startCount = false
	var stopCount = false
    if (asc) {      
        a = 0 
        b = 1
        c = 1
        d = n     
    }
    else {
	a = 1
        b = 1
	c = n-1
	d = n     
    }
    println(a + "/" + b)
    while ((asc && c <= n) || (!asc && a > 0)) {
	//i += 1
	val k = scala.math.floor((1.*n + b)/d).toInt
	val ao = a
	val bo = b
	a = c
        b = d
	c = k*c - ao
	d = k*d - bo
        //if (b > 30) 
       //println(a + "/" + b)
       if (a == 1 && b == 3) startCount = true
       if (a == 1 && b == 2) stopCount = true
       if (startCount && !stopCount) count += 1
    }
  count
}


countfarey(8,true)
