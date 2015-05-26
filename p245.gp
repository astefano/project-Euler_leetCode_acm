\\forprime(p=3, 10^11, forprime(q = nextprime(p+1), 2*10^11/p, ((p*q-1)%(p+q-1) | print(p" "q" "p*q)););)

\\ k < (2*10^11)^(1/2) = sqrt(2)*10^5.5 
\\forstep(k = 2, 447214, [2], tf = k*k-k+1; fordiv(tf, f1, q=k+f1; p=k+tf/f1; (isprime(q) == 0 || isprime(p) == 0 || p > q || p*q > 2*10^12) & next; print1(p*q" "p" "q"\n")))	

\\forstep(k = 2, 5849, [2], forstep(ps = 3, 7*10^6, [2], tf = k*k*ps-k*ps+1; fordiv(tf, f1, p3=k+f1; p12=k*ps+tf/f1; (isprime(p3) == 0 || numdiv(p12) > 4 || ps != sumdiv(p12,x,x)-p12-2 || p3*p12 > 2*10^12) & next; print1(k" "p3*p12" "p3" "factor(p12)" = "p12" "ps"\n"))))

\\forstep(k = 2, 5849, [2], forstep(ps = 3, 7*10^6, [2], tf = k*k*ps-k*ps+1; f1 = divisors(tf)[2]; p3=k+f1; p12=k*ps+tf/f1; (isprime(p3) == 0 || numdiv(p12) > 4 || ps != sumdiv(p12,x,x)-p12-2 || p3*p12 > 2*10^12) & next; print1(k" "p3*p12" "p3" "factor(p12)" = "p12" "ps"\n")))

forstep(k = 2, 5849, [2], forstep(ps = 3, 5*10^6, [2], tf = k*k*ps-k*ps+1; fordiv(tf, f1, p3=k+f1; p12=k*ps+tf/f1; divp12 = divisors(p12); (isprime(p3) == 0 || numdiv(p12) > 4 || ps != divp12[2]+divp12[3]-1 || p3*p12 > 2*10^12) & next; print1(k" "p3*p12" "p3" "factor(p12)" = "p12" "ps" "f1" "factor(tf)"\n"))))

\\forstep(k = 2, 5849, [2], forstep(ps = 3, 2*10^11/k^2, [2], tf = k*k*ps-k*ps+1; fordiv(tf, f1, p3=k+f1; p12=k*ps+tf/f1; (isprime(p3) == 0 || numdiv(p12) > 4 || ps != sumdiv(p12,x,x)-p12-2 || p3*p12 > 2*10^12|| 2*p3 > ps) & next; print1(k" "p3*p12" "p3" "factor(p12)" = "p12" "ps"\n"))))


quit



