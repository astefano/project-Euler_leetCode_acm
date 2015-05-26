#represent n as \sum_{i=1} v[i] * p^(i - 1)
pRepr(n,p) = { cn = n; cp = floor(log(cn)/log(p)); v = vector(cp+1); while( cn > 0, nit+=1; v[cp+1]=floor(cn/(p^cp)); cn = cn - v[cp+1]*p^cp; if (cn > 0, cp = floor(log(cn)/log(p)))); v }

verif(n,p) = {v=pRepr(n,p); print(v); r=0; for(i=1, #v, r+=p^(i-1)*v[i]); n == r}

#the number of binomial coeffs on line n (in Pascal triangle) which aren't divisible by p is \Pi_i (v[i]+1) where v is the p-ary repr of n
t(n,p)={v=pRepr(n,p); r=1; for(i=1, #v, r*=(v[i]+1)); r}

t(n,p)={r=1; cn = n; cp = floor(log(cn)/log(p)); while( cn > 0, nit+=1; v=floor(cn/(p^cp)); cn = cn - v*p^cp; if (cn > 0, cp = floor(log(cn)/log(p))); r*=(v+1)); r}

st(n,p)= {s=0; for(i=1, n, s+=t(i, p)); s}

st(10^9-1, 7)+1


788306977696
311840600113336

