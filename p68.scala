 def sols3gon(s: Int) = for(a1 <- List.range(1,7); a2 <- List.range(1,7).filter(_!=a1); val a3 = s - a1 - a2; a4 <- List.range(1,7); val a6 = a1 + a2 - a4; val a7 = s - a1 - 2*a2 + a4; if (List(a1,a2,a3,a4,a6,a7).sorted == List.range(1,7) && a1 < a4 && a1 < a7)) yield List(List(a1,a2,a3), List(a4,a3,a6), List(a7,a6,a2))

sols(9).foldLeft("")(_+"\n"+_)

def sols5gon(s: Int) = for(a1 <- List.range(1,11); a2 <- List.range(1,11).filter(_!=a1); val a3 = s - a1 - a2; a4 <- List.range(a1+1,11); val a5 = a1 + a2 - a4; a6 <- List.range(a1, 11); val a7 = s - a5 - a6; a8 <- List.range(a1, 11); val a9 = s - a8 - a7; val a10 = s - a2 - a9; if (List(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10).sorted == List.range(1,11) && a1 < List(a4,a6,a8,a10).min)) yield List(List(a1,a2,a3), List(a4,a3,a5), List(a6,a5,a7), List(a8,a7,a9), List(a10,a9,a2))


List.range(9,30).filter(i => solsP68(i) != List())
//res387: List[Int] = List(14, 16, 17, 19)


List(14,16,17,19) map (x => solsP68(x).foldLeft("")((r,c) => r + "\n"+ c.map(_.mkString).mkString))
