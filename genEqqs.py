  def eqInteraction(alpha):
      indices = range(alpha.length)
    eqs0 = [hcname + alpha(i) + " == " + hcname + alpha(j) for(i <- indices; j <- indices.dropWhile(_<=i))]
    val eqs = eqs0.toList
    if (eqs == List()) "" 
    else if (eqs.length == 1) eqs(0)
    else eqs.tail.foldLeft("And(" + eqs(0))((r, c) => r + ", " + c) + ")"
  }

  def leqInteraction(alpha: List[String], im: List[List[String]]) = {
    //the name of the history clock
    val a = alpha.head
    val portsIM = im.flatten.distinct
    val ineqs = for(p <- portsIM) yield hcname + a + " <= " + hcname + p
    if (ineqs.length > 1) 
      ineqs.tail.foldLeft("And(" + ineqs(0))((r, c) => r + ", " + c) + ")"
    else ineqs.mkString
  }

  /** returns \gamma \ominus \alpha as on the slides. 
    */ 
  def imdiff(alpha: List[String], im: List[List[String]]) = im.map{el => (el.toSet -- alpha.toSet).toList}.filter{el => el.length >= 1}
  

  # returns \eqs(\gamma) as on the slides. it makes use of disjointness, s.t. the resulting formulae are minimal in size.
  def getEqsSimpl(im: List[List[String]]) : String = {
    //println("#im = " + im)
    if (im == List()) ""
    else if (im.length <= 1 && im.head != eps) eqInteraction(im.head) 
    else {   
      val alpha = im.head
      val (p1, p2) = im.tail.partition(alphai => (alphai.toSet intersect alpha.toSet) == Set())
      if (p1 != List()) {
	val p1Eqs = getEqsSimpl(p1)
	val p2Eqs = getEqsSimpl(alpha +: p2)
	if (p1Eqs != "") {
	  if (p2Eqs != "")
	    "And(" + p1Eqs + ", " + p2Eqs + ")"
	  else p1Eqs
	}
	else p2Eqs
      }
      else {
	val res = for { 
	  alpha <- im; 	
	  val eqAlpha = eqInteraction(alpha); 
	  val leq = leqInteraction(alpha, imdiff(alpha, im));
	  val rem = getEqsSimpl(imdiff(alpha, im).filter{_.length > 1});
	  val all = List(eqAlpha, leq, rem).filter(_!="");
	  if (all != List())
	} //yield "And(" + (if (eqAlpha != "") + (if (leq != "") (", " + leq) else "") + (if (rem != "") (", " + rem) else "")) + ")"
	yield "And(" + all.reduceLeft(_+ ", " +_) + ")"
	"Or(" + res.tail.foldLeft(res(0))((r,c) => r + ", " + c) + ")"
      }
    }
  }
