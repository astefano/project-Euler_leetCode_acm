object P71 {

  def p71(lim: Int) = {
    var d = 5
    var n = 2
    while(d < lim) {
	d = d + 7
	n = n + 3
    }
    if (d > lim) (n - 3, d - 7) else (n, d)
  }

//takes light years
def farey(n : Int, asc : Boolean ) = {
// Adapted from """Python function to print the nth Farey sequence, either ascending or descending."""
	var i = 0 
        var a = 0 
        var b = 1
        var c = 1
        var d = n         
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
    var stop = false
    while (((asc && c <= n) || (!asc && a > 0)) && !stop) {
	//i += 1
	val k = scala.math.floor((1.*n + b)/d).toInt
	val ao = a
	val bo = b
	a = c
        b = d
	c = k*c - ao
	d = k*d - bo
	if (c == 3 && d == 7) {
       println(a + "/" + b)
	  stop = true 
	}
    }
  }

def main(args: Array[String]) {
farey(1000000,true)
}

}
