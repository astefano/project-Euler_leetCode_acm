object P54 {

  import scalax.io._
  import scalax.file.{ FileOps, Path, NotFileException }

//encode 2, 3, 4, 5, 6, 7, 8, 9, 10, Jack, Queen, King, Ace as List.range(2,15)

  def load = {
    var handsP1 = List[Set[(Char, Int)]]()
    var handsP2 = List[Set[(Char, Int)]]()
    val file = "/media/ubuntuPart2/docs/topcoder/poker.txt"
    val rf: Input = Resource.fromFile(file)
    val lines = rf.lines().filter(_!="")
    val nlines = lines.size
    println("n lines = " + nlines)
    lines foreach {
      l => 
	val hands = l.trim.split(" ").filter(_!="").toList.map{e => (e(1), if (e(0)-'0' < 10) (e(0)-'0') else if (e(0) == 'A') 14 else if (e(0) == 'K') 13 else if (e(0) == 'Q') 12 else if (e(0) == 'J') 11 else 10)}
      handsP1 = handsP1 :+ hands.take(5).toSet
      handsP2 = handsP2 :+ hands.drop(5).toSet
    }
    (handsP1, handsP2)
  }

  def handWithMultiplicities(hand: Set[(Char, Int)]) = {
    val valL = hand.toList.map{_._2}
    hand.map{_._2}.map{v => (v, valL.filter(_==v).length)}
  }

  def highCard(hand: Set[(Char, Int)]) = hand.map{_._2}.max

  def hasOnePair(hand: Set[(Char, Int)]) = {
    val aux = handWithMultiplicities(hand)
    val pairs = aux.filter(_._2==2)
    pairs.size > 0
  }

  def hasTwoPairs(hand: Set[(Char, Int)]) = {
    val aux = handWithMultiplicities(hand)
    val pairs = aux.filter(_._2==2)
    pairs.size > 1
  }

  def getPairs(hand: Set[(Char, Int)]) = {
    val aux = handWithMultiplicities(hand)
    val pairs = aux.filter(_._2==2)
    pairs
  }

  def hasThreeOfKind(hand: Set[(Char, Int)]) = {
    val aux = handWithMultiplicities(hand)
    val pairs = aux.filter(_._2==3)
    pairs.size > 0
  }

  def hasStraight(hand: Set[(Char, Int)]) = {
    val vals = hand.map{_._2}.toList.sorted
    (vals.length == 5 && !List.range(0,4).exists(i => vals(i+1)-vals(i) != 1))
  }

  def hasFlush(hand: Set[(Char, Int)]) = hand.map{_._1}.size == 1

  def hasFullHouse(hand: Set[(Char, Int)]) = hasOnePair(hand) && hasThreeOfKind(hand)
  
  def hasFourOfKind(hand: Set[(Char, Int)]) = {
    val aux = handWithMultiplicities(hand)
    val pairs = aux.filter(_._2==4)
    pairs.size == 1
  }

  def hasStraightFlush(hand: Set[(Char, Int)]) = hasFlush(hand) && hasStraight(hand)

  def hasRoyalFlush(hand: Set[(Char, Int)]) = hasFlush(hand) && hand.map{_._2} == Set(10,11,12,13,14)

  def getRank(hand: Set[(Char, Int)]) = {
    if (hasRoyalFlush(hand)) 10
    else if (hasStraightFlush(hand)) 9
    else if (hasFourOfKind(hand)) 8
    else if (hasFullHouse(hand)) 7
    else if (hasFlush(hand)) 6
    else if (hasStraight(hand)) 5
    else if (hasThreeOfKind(hand)) 4
    else if (hasTwoPairs(hand)) 3
    else if (hasOnePair(hand)) 2
    else 1
  }

  def main(args: Array[String]) {
    val r = load
    val p1 = r._1
    val p2 = r._2
    val ngames = 1000

    def countWinners = {
      var w1 = 0    
      List.range(0,ngames) foreach {
	g => 
	  val r1 = getRank(p1(g))
	  val r2 = getRank(p2(g))
	  println("at g: " + (g+1) + " p1 = " + p1(g) + " has rank " + r1 + " p1 = " + p2(g) + " has rank " + r2 + " w1 =" + w1)
	if (r1 > r2) w1 += 1
	else if (r1 == r2) {
	  if (r1 == 2) {
	    val pairs1 = getPairs(p1(g)).map{_._1}
 	    val pairs2 = getPairs(p2(g)).map{_._1}
	    if ((pairs1.max > pairs2.max) || ( pairs1.max == pairs2.max && (p1(g).map{_._2} - pairs1.max).max > (p2(g).map{_._2} - pairs2.max).max ) ) w1 += 1
	    println("r1 = r2 = 2: " + " pairs = " + ((pairs1.max, pairs2.max))  + " w1 = " + w1) 
	  }
	  else if (r1 == 3) {
	    val pairs1 = getPairs(p1(g)).map{_._1}
 	    val pairs2 = getPairs(p2(g)).map{_._1}
	    if ((pairs1.max > pairs2.max) || ( pairs1.max == pairs2.max && (pairs1 - pairs1.max).max > (pairs2 - pairs2.max).max)) w1 += 1
	    println("r1 = r2 = 3: " + " pairs = " + ((pairs1.max, pairs2.max))  + " w1 = " + w1) 
	  }
	  else if (r1 == 7) {
	    //full house
	    //check value of ThreeOfKind
	    val pair1 = getPairs(p1(g)).map{_._1}
	    val pair2 = getPairs(p2(g)).map{_._1}
	    val k13 = p1(g).map{_._2} - pair1.max
	    val k23 = p2(g).map{_._2} - pair2.max
	    if ((k13.max > k23.max) || (k13.max == k23.max && pair1.max > pair2.max)) w1 += 1
	    println("r1 = r2 = 7: " + " (k13,k23) = " + ((k13.max, k23.max)) + " pairs = " + ((pair1.max, pair2.max))  + " w1 = " + w1) 
	  }
	    else {
	      var v1 = p1(g).map{_._2}
	      var v2 = p2(g).map{_._2}
	      while (v1.max == v2.max) { 
		v1 = v1 - v1.max
		v2 = v2 - v2.max
	      }
	      if (v1.max > v2.max) w1 += 1
	      println("r1 = r2: checking max value.")
	    }
	}
      }
      w1
    }
    println(countWinners)
  }
}

