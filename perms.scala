object Perms {

  def main(args: Array[String]) {
    println(List('X','Y','Z','U','V','W').permutations.toList.foldLeft("")(_+"\n"+_))

  }

}

