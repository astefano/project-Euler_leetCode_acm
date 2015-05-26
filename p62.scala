object P62 {

  def main(args: Array[String]) {

    val cubes = List.range(101, 30000) map (x => BigInt(x)*BigInt(x)*x)

    val sc = cubes.map{
      x => 
	val y = x.toString
	val ys = y.toSet
        (x, ys.map{c => (c, y.filter(_==c).length)})}.sortBy(x => x._2.size)

    val distinct = (sc map (x => x._2)).toSet

    val sc2 = (sc map (x => x._2))

    val res5 = (distinct map (s => (s, (sc2.filter(_==s).length)))).filter(_._2 == 5)

    println(res5)

    println(res5 map (x => sc.filter(_._2 == x._1).map(_._1).sorted))
  }
  
}
