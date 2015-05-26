\\for(n=280, 10000, for(m=160, 10000, aux1=m+n; aux2=1+n-m; f=gcd(aux1,aux2); ( (aux1%2==0 && (!issquare(aux1/f/2) || !issquare(aux2/f))) || (aux2%2==0 && (!issquare(aux1/f) || !issquare(aux2/f/2))) ) & next; print1(n" "m" "n*(n+1)/2"\n"));)

for(n=2, 100000, for(m=2, 100000, ( n*(n+1)!=m*(3*m-1) ) & next; print1(n" "m" "n*(n+1)/2"\n"));)

quit
