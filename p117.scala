object P117 {

def exps(n: Int) = for(b <- (0 to n/4); v <- (0 to n/3); r <- (0 to n/2); val w = n-2*r-3*v-4*b; if (w >= 0)) yield "[" + w + ", " + r + ", " + v + ", " + b + "]"

def main(args: Array[String]) {
  println(exps(5))
}

}
