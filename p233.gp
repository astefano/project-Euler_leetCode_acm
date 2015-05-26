n = 10^11

s = 0

forprime(p3=5, (n)^(1./3), !(p3 % 4 == 1) & next; forprime(p2=5, sqrt(n/p3^3), !(p2 % 4 == 1 & p3 != p2) & next; forprime(p1=5, n/(p2^2*p3^3), p=p1*p2^2*p3^3; q=floor(n/p); !(p1 % 4 == 1 & p3 != p1 & p2 != p1 & q > 0) & next; for(qi=1, q, facts=factor(qi); e=facts[,1]; j=0; for(i=1, #e, if(e[i]%4==1, break); j=i); (j!=#e) & next; s+=qi*p))))

print1("\ns = "s"\n")

forprime(p2=5, n^(1./10), !(p2 % 4 == 1) & next; forprime(p1=5, sqrt(n/p2^10), !(p1 % 4 == 1 & p2 != p1) & next; p=p1^2*p2^10; q=floor(n/p); for(qi=1, q, facts=factor(qi); e=facts[,1]; j=0; for(i=1, #e, if(e[i]%4==1, break); j=i); (j!=#e) & next; s+=qi*p)))

print1("\ns = "s"\n")

forprime(p2=5, n^(1./7), !(p2 % 4 == 1) & next; forprime(p1=5, (n/p2^7)^(1./3), !(p1 % 4 == 1 & p2 != p1) & next; p=p1^3*p2^7; q=floor(n/p); for(qi=1, q, facts=factor(qi); e=facts[,1]; j=0; for(i=1, #e, if(e[i]%4==1, break); j=i); (j!=#e) & next; s+=qi*p)))

print1("\ns = "s"\n")

quit
