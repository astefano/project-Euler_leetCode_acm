c = 0 

forstep(n=274646,15*10^5,2,j=0; facts=factor(n); e=facts[,1]; j=0; for(i=1, #e, !(e[i]%4==1) & next; j+=1); (j>=1) & next; j=0; oy=0;ox=0; for(y=2, n/2-1, x = (n*(n-2*y)) / (2*(n-y)); !( (n*(n-2*y)) % (2*(n-y)) == 0 & x < y & issquare(x^2+y^2)) & next; oy=y; ox=x; j+=1; if (j > 1, break)); !(j==1) & next; c+=1; print1(n" "factor(n)" "oy" "ox" "n-ox-oy"\n"))

print1("c="c"\n") 

quit
