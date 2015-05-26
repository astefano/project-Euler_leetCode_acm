for(n = 76967, 10^6, fibn = Vec(Str(fibonacci(n))); f9=vecsort(vecextract(fibn,"1..9")); l9 = vecsort(vecextract(fibn,"-1..-9")); !(f9 == l9 && f9 == ["1", "2", "3", "4", "5", "6", "7", "8","9"]) & next; print1(n))

quit


