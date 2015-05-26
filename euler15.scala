def nroutes(dim : Int) = {
  
  val m = Array.fill(dim+1, dim+1)(0L)
  for (i <- List.range(0, dim + 1)) {
    m(i)(0) = 1L
    m(0)(i) = 1L
    }

  for (i <- List.range(1, dim + 1); j <- List.range(1, dim + 1)) {
    //m(i)(j) = 2L * List.range(0, j).foldLeft(0L)((r, c) => (r + m(i - 1)(c)))
    m(i)(j) = m(i - 1)(j) + m(i)(j - 1)
  }

  m(dim)(dim)
}
