s(k) = 3*k*k-3*k+2

isValid(k) = { isprime(s(k+1)+1-s(k)) && isprime(s(k+1)-1-s(k)) && isprime(s(k+2)-1-s(k)) }

isValid2(k) = { isprime(s(k+2)-1-s(k)) && isprime(s(k+2)-1-s(k+1)) && isprime(s(k+3)-2-s(k+2)+1) }

nf = 2; for(k=1,70000,if (isValid(k), print1("at nf1 ="nf" : "s(k)"\n"); nf+=1); if(isValid2(k), print1("at nf2 ="nf" : "s(k+1)-1"\n"); nf+=1))

