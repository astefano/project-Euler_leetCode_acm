
val primes46 = primes.drop(1)

while(!found) {
      val sp = primes46.takeWhile(_<=n)
      if (sp.last < n) {
      val res = sp.filter{p =>  
      val aux = (n - p)/2
      val aux2 = math.floor(math.sqrt(aux))
      (aux2*aux2 == aux)}.length
      if (res == 0) found = true
      }
      n += 2
      }

