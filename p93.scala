object P93{

  sealed abstract class Expression
  case class V(x: Int) extends Expression
  case class binOp(op: String, left : Expression, right : Expression) extends Expression
/*
  case class Const(value : Int) extends Expression
  case class Add(left : Expression, right : Expression) extends Expression
  case class Minus(left : Expression, right : Expression) extends Expression
  case class Mult(left : Expression, right : Expression) extends Expression
  case class Div(left : Expression, right : Expression) extends Expression

  def eval(expression : Expression) : Int = expression match {
    case V(x) => x
    case Const(cst) => cst
    case Add(left, right) => eval(left) + eval(right)
    case Minus(left, right) => eval(left) - eval(right)
    case Mult(left, right) => eval(left) * eval(right)
    case Div(left, right) => eval(left) / eval(right)
  }

  def gen(a: Int, b: Int, c: Int, d: Int) = {
    val ops = Set("Add","Minus","Mult","Div")
    for(op1 <- ops; op2 <- ops-op1; op3 <- ops-op1-op2) yield Set(op1+"("+op2+"(V("+a+"),V("+b+")),"+op3+"(V("+c+"),V("+d+")))",
								  op1+"("+op2+"("+op3+"(V("+a+"),V("+b+")),V("+c+")),V("+d+"))"
								)
  }
*/

  def eval(expression : Expression) : Int = expression match {
    case V(x) => x
    case binOp(op, l, r) => op match {
      case "+" => eval(l) + eval(r)
      case "-" => eval(l) - eval(r)
      case "*" => eval(l) * eval(r)
      case "/" => {
	val er = eval(r)
	val el = eval(l)
	if (er != 0 && el % er == 0) el / er else -20000	
      }
    }
  }

  def genEval(a: Int, b: Int, c: Int, d: Int) = {
    val ops = Set("+","-","*","/")
    val r = for(op1 <- ops; op2 <- ops; op3 <- ops) yield Set(eval(binOp(op1, binOp(op2,V(a),V(b)), binOp(op3,V(c),V(d)))),
								  eval(binOp(op1, binOp(op2, binOp(op3, V(a),V(b)),V(c)),V(d)))
								  //eval(binOp(op1, V(a), binOp(op2, V(b), binOp(op3, V(c),V(d)) ) ) ),
								  //eval(binOp(op1, binOp(op2, V(a), binOp(op3, V(b),V(c))),V(d))),
								  //eval(binOp(op1, V(a), binOp(op2, binOp(op3, V(b),V(c)),V(d))))
								)
    r.flatten.filter(x => x>0 && x<10000)
  }

  def gen(a: Int, b: Int, c: Int, d: Int) = {
    val ops = Set("+","-","*","/")
    val r = for(op1 <- ops; op2 <- ops; op3 <- ops) yield Set(binOp(op1, binOp(op2,V(a),V(b)), binOp(op3,V(c),V(d))),
								  binOp(op1, binOp(op2, binOp(op3, V(a),V(b)),V(c)),V(d))
								  //binOp(op1, V(a), binOp(op2, binOp(op3, V(b),V(c)),V(d)))
								)
    r.flatten//.filter(_>0)
  }

  def main(args: Array[String]) {
    val cand = List.range(0,10).combinations(4).toList 
    var vmax = 28
    var mini = "1234"
    cand foreach {
      ini => 
	val r = for(a<-ini; b<-ini-a; c<-ini-a-b; d<-ini-a-b-c) yield genEval(a,b,c,d)    
	val r1 =  r.flatten.toSet
	val aux = List.range(1,vmax+1).toSet -- r1
	var i = 0
	if (aux == Set()) {
	  val r2 = r1.toList.sorted
	  i = r2.indexOf(vmax) + 1 
	  if (i > 0) {
	    //println("i = " + i)
	    var old = vmax
	    var stop = false
	    while (i < r2.length && !stop) {
	      if (r2(i) > old + 1) stop = true
	      if (!stop) {
		old = r2(i)
		i+=1
	      }
	    }
	    if (stop) vmax = r2(i-1) else vmax = r2.last
	    mini = ini.mkString
	  }
	}
	println("ini = " + ini  + " mini = " + mini + " vmax = " + vmax + " i = " + i + " gen = " + r1.toList.sorted)
    }
    mini
  }

}
