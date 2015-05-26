import swing._
//import javax.swing._
import java.awt.geom.Line2D
import java.awt.{RenderingHints, BasicStroke, Color}

object P220 extends SimpleSwingApplication {
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

  def top = new MainFrame {
    contents = new Panel {
      preferredSize = new Dimension(600, 500)

      override def paintComponent(g: Graphics2D) {
	
	var count = 0L

	g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
        //g.setStroke(new BasicStroke(DEPTH))           
	g.setColor(Color.green)

        //DrawDragonLine( DEPTH, true, 100, 100, 0, 100)
	//DrawDragonLine( DEPTH, true, 0, 0, 0, 100)
	//DrawDragonLine( DEPTH, false, 200, 200, 200, 0)
	//DrawDragonLine( DEPTH, true, 200, 200, 200, 0)
	//DrawDragonLine( DEPTH, true, 200, 200, 200, 300)
	//DrawDragonLine( DEPTH, true, 100, 100, 100, 0)
	//DrawDragonLine( DEPTH, true, 100, 100, 0, 100)
	//DrawDragonLine( DEPTH, false, 200, 200, 200, 0)
	//DrawDragonLine( DEPTH, false, 0, 0, (312.5).toFloat, 0)
	DrawDragonLine( DEPTH, false, 0, 0, 100, 0)
	println("at fin, count = " + count)

	def DrawDragonLine(depth: Int, turn_towards: Boolean, x1: Float, y1: Float, dx: Float, dy: Float) { 
	  //if (turn_towards) g.setColor(Color.red) else g.setColor(Color.blue)
	  if (depth <= 0) {
	    val l = new Line2D.Double(x1, y1, x1 + dx, y1 + dy)
	    //if (count == 1024-500) {
	    if (count == 500) {
	      println(" count = " + count + " x1 = "+ x1 + " dx = " + dx + " y1 = " + y1 + " dy = " + dy + " turn_towards = " + turn_towards)
	      g.setColor(Color.red)
	    }
	    if (count >= 0 && count < 500) {
	      //println(" count = " + count + " x1 = "+ x1 + " dx = " + dx + " y1 = " + y1 + " dy = " + dy + " turn_towards = " + turn_towards)
	      g.setColor(Color.black)
	    }
	    if (count > 500) g.setColor(Color.blue)
	    if (count < 100) println(" count = " + count + " x1 = "+ x1 + " dx = " + dx + " y1 = " + y1 + " dy = " + dy + " turn_towards = " + turn_towards)
            g.draw(l)
	    count += 1
	  }
	  else
	    {
              val nx = (dx / 2)
              val ny = (dy / 2)
              val dx2 = -ny + nx
              val dy2 = nx + ny
              if (turn_towards)
		{
		  // Turn to the right.
		  //DrawDragonLine(depth - 1, true, x1, y1, dx2, dy2)
		  DrawDragonLine(depth - 1, false, x1, y1, dx2, dy2)

		  val x2 = x1 + dx2
		  val y2 = y1 + dy2
		  //DrawDragonLine(depth - 1, false, x2, y2, dy2, -dx2)
		  DrawDragonLine(depth - 1, true, x2, y2, dy2, -dx2)
		}
              else
		{
		  // Turn to the left.
		  //DrawDragonLine(depth - 1, true, x1, y1, dy2, -dx2)
		  DrawDragonLine(depth - 1, false, x1, y1, dy2, -dx2)
		  val x2 = x1 + dy2
		  val y2 = y1 - dx2
		  //DrawDragonLine(depth - 1, false, x2, y2, dx2, dy2)
		  DrawDragonLine(depth - 1, true, x2, y2, dx2, dy2)
		}
	    }
	}
      }
    }
  }
}
