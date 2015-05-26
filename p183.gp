maxP(n)={m = n; for(k=2,n-1, r = n/k; p = r^k; if (m < p, m = p)); m}
s25 = setunion(Set(2),Set(5))
md(n)={r = maxPF(n); den = denominator(r); f = Set(factorint(den)[,1]); if (setminus(f, s25) == Set(), -n, n)}
p183(n)=sum(k=5, n, md(k))

maxPFO(n)={mold = n; k = 2; mnew = (n/k)^k; while(mold < mnew, k+=1; mold=mnew; mnew = (n/k)^k); denominator(n/(k-1))}
md(n)={r = maxPF(n); f = Set(factorint(r)[,1]); if (setminus(f, s25) == Set(), -n, n)}
p183(n)=sum(k=5, n, md(k))

maxPF(n)={mold = n; k = floor(n/2); mnew = (n/k)^k; while(mold < mnew, k-=1; mold=mnew; mnew = (n/k)^k); denominator(n/(k+1))}

