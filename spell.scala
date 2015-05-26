object spell {

  import scalax.io._
  import scalax.file.{ FileOps, Path, NotFileException }

  val mld = Map(('a',"2"), ('b', "22"),  ('c', "222"),
		('d',"3"), ('e', "33"),  ('f', "333"),
		('g',"4"), ('h', "44"),  ('i', "444"),
		('j',"5"), ('k', "55"),  ('l', "555"),
		('m',"6"), ('n', "66"),  ('o', "666"),
		('p',"7"), ('q', "77"),  ('r', "777"), ('s', "7777"),
		('t',"8"), ('u', "88"),  ('v', "888"),
		('w',"9"), ('x', "99"),  ('y', "999"), ('z', "9999"),
		(' ',"0"))
  //val letters = ('a' to 'z')
  //val mapLetterDigits = letters.map(l => (l, ))

  def main(args: Array[String]) {
    val start = System.currentTimeMillis

    //val file = "testSpell.in"   
    //val file = "C-small-practice.in"   
    val file = "C-large-practice.in"   
    val rf: Input = Resource.fromFile(file)

    //val fileOut = "C-small-practice.out"   
    val fileOut = "C-large-practice.out"   
    val rfo: Output = Resource.fromFile(fileOut)

    val lines = rf.lines().filter(_!="")
    val nlines = lines.size
    var out = ""

    val ncases = lines(0).toInt
    (0 to ncases-1) foreach {
      i => 
	out += "Case #"+(i+1)+": " + lines(i+1).toList.map(c => mld(c)).reduceLeft((r,c)=>( if (r.last == c.head) (r + " "+  c) else (r + c))) + "\n"
    }
    //println(out)
    rfo.write(out)
    println( "Time (s): " + ( System.currentTimeMillis() - start ) / 1000. )
  }

}
