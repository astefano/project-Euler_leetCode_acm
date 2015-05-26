object P220 {
  var DEPTH = 10
  val d0 = "Fa"
  def ra(s: String) = s.replace("a", "aRbFR")
  def rb(s: String) = s.replace("b", "LFaLb")

/*
a=90° 
axiom: L 
L→L+R+ 
R→-L-R

It works, but you need to introduce some multiple of a 45° angle at
the beginning of the drawing at each step, in order to obtain curves
that match each other. If not, the successive curves turn around the
starting point. So I prefer to use this L-system:


a=45° 
axiom: L 
L→-L++R- 
R→+L—R+   
*/

/*
 * "X" -> "X+YF+",
 * "Y" -> "-FX-Y"
 */ 

/* L-System:
 * Angle 45
 * Axiom FX
 * F —> Z
 * X —> +FX−−FY+
 * Y —> −FX++FY−
 */ 

  var count = 0L

  var LIM = 1000000000000L

  var DEBUG = false

  val ct = 3.125

  val sqrt2 = 1.41421356237

  var cont = true

  def main (args: Array[String]) {
    DEPTH = args(0).toInt
    if (args.length > 1 && args(1).toLong > 0) LIM = args(1).toLong
    if (args.length > 2) DEBUG = true
    val start = System.currentTimeMillis
    dragonLine( DEPTH, false, 0, 0, math.pow(sqrt2, DEPTH).toFloat, 0)
    println("at fin, count = " + count)
    println( "Time (s): " + ( System.currentTimeMillis() - start ) / 1000. )
  }

  def dragonLine(depth: Int, turn_towards: Boolean, x1: Float, y1: Float, dx: Float, dy: Float) { 
    if (cont) {
    if (depth <= 0) {
      //if (count == LIM) println((x1/ct) + " " + (math.abs(y1)/ct))	      
      if (count == LIM) {
	println("at LIM = " + x1 + " " + y1)
	cont = false
      }
      if (count < 30 && DEBUG) println(x1 + " " + y1)
      count += 1
    }
      else {
        val nx = (dx / 2)
        val ny = (dy / 2)
        val dx2 = -ny + nx
        val dy2 = nx + ny
        if (turn_towards) {
	  dragonLine(depth - 1, false, x1, y1, dx2, dy2)
	  val x2 = x1 + dx2
	  val y2 = y1 + dy2
	  dragonLine(depth - 1, true, x2, y2, dy2, -dx2)
	}
        else {
	  dragonLine(depth - 1, false, x1, y1, dy2, -dx2)
	  val x2 = x1 + dy2
	  val y2 = y1 - dx2
   	  dragonLine(depth - 1, true, x2, y2, dx2, dy2)
	}
      }
    }
  }
}
    
