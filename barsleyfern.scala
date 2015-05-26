
/*
 * from http://cyberroadie.wordpress.com/2012/06/27/a-scala-fractal/
 */ 


import java.awt.{Color, Graphics2D, Dimension}
import swing.{SimpleSwingApplication, Panel, Frame}
import util.Random
class FernFractalFrame(transformFunction: (Double, Double) => (Double, Double), val width: Int, val height: Int, val max: Int) extends Frame {
 
contents = new Panel {
 preferredSize = new Dimension(width, height)
 opaque = true
 
override def paint(g: Graphics2D) {
 g.setBackground(new Color(0, 0, 0))
 g.setColor(Color.GREEN)
 g.drawLine(height / 2, width / 2, height / 2, width / 2)
 drawFern((0, 1), max, g)
 }
 
def paintPoint(p: (Double, Double), g: Graphics2D) = {
 val scale = height / 11
 val y = (height - 25) - (scale * p._2)
 val x = (width / 2) + (scale * p._1)
 g.drawLine(x.toInt, y.toInt, x.toInt, y.toInt)
 }
 
def drawFern(p: (Double, Double), max: Int, g: Graphics2D) {
 paintPoint(p, g)
 repaint()
 if (max != 0)
 drawFern(transformFunction(p._1, p._2), max - 1, g)
 }
 }
}
 
object FernFractal extends SimpleSwingApplication {
 
object TransformFunction extends CFernTransformFunction
 
class CFernTransformFunction extends ((Double, Double) => (Double, Double)) {
 
def rnd = Random.nextInt(100)
 
def transformPoint(p: (Double, Double), a: Double, b: Double, c: Double, d: Double, s: Double): (Double, Double) =
 ((a * p._1) + (b * p._2), ((c * p._1) + (d * p._2) + s))
 
def apply(x: Double, y: Double) = {
 rnd match {
 case n if n <= 1 => transformPoint((x, y), 0.0, 0.0, 0.0, 0.16, 0.0)
 case n if n <= 7 => transformPoint((x, y), 0.2, -0.26, 0.23, 0.22, 1.6)
 case n if n <= 14 => transformPoint((x, y), -0.15, 0.28, 0.26, 0.24, 0.44)
 case n if n <= 100 => transformPoint((x, y), 0.85, 0.04, -0.04, 0.85, 1.6)
 }
 }
 }
 
def top = new FernFractalFrame(TransformFunction, 400, 400, 10000)
}
