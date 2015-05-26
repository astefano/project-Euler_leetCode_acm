mmax = 2

v = 0

for(n=1,10^6, l=1; f=sigma(n)-n; visited=Set(n); while(f<10^6 && f!=n && f!=0 && !setsearch(visited,f),visited=setunion(visited, Set(f));f=sigma(f)-f;l=l+1); oldm = mmax; !(f==n && mmax < l) & next; mmax=l; print1(n" "f" "mmax" "l"\n"))

quit

